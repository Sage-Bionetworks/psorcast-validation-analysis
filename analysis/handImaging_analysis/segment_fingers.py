###############################################
# Script used for segmenting fingers to get the zoomed nail view
# it will output Images as a File Entity in Synapse Folder
# and generate a Table Entity
# Author: Aryton Tediarjo, Meghasyam Tummalacherla
# Maintainer: aryton.tediarjo@sagebase.org
#############################################

# import required libraries
import sys
import os
import shutil
import cv2 as cv # opencv
import numpy as np # for numerical calculations 
import synapseclient # synapse login etc
import synapseutils # to download files using folder structure
import pandas as pd # data frames
from matplotlib import pyplot as plt # plotting
import mediapipe as mp # for detecting hand landmarks
import timeit # to track program running time
from synapseclient import Column, Schema, Table, Activity, File

# Global Variables
FULL_RES_IMAGE_MAPPING_FOLDER_ID = 'syn25837496'
FUSE_IMAGE_MAPPING_FOLDER_ID = 'syn25999658'
SEGMENTED_NAIL_OUTPUT_ID = "syn25999657"
NAILS_RECT_OUTPUT_ID = 'syn22342373'
OUTPUT_DIRECTORY = 'hand_segmentation_psorcast Jun 16 2021 08_51 Dan/'
TARGET_DIRECTORY = 'testHulls' 
TARGET_NAILS_DIR = 'segmentedNails'
NAILS_RERCT_OUTPUT_FILE = 'nail_bounding_rects.tsv'

## Table Global Variables
TABLE_NAME = "HandImaging-FingerSegmentation"
PARENT_ID = "syn22276946"
GIT_URL = "https://github.com/arytontediarjo/psorcast-validation-analysis/blob/model/analysis/handImaging_analysis/segment_fingers.py"

# mediapipe objectt
mp_drawing = mp.solutions.drawing_utils
mp_hands = mp.solutions.hands

# login into Synapse
syn = synapseclient.login()

## hand landmark detection using mediapipe
hands = mp_hands.Hands(static_image_mode=True,
                       max_num_hands=1, # use one hand at a time 
                       min_detection_confidence=0.5)

# Function to create required directory
def create_directories():
    directories = [
        "testHulls/left",
        "testHulls/right",
        "segmentedNails/left",
        "segmentedNails/left_unrotated",
        "segmentedNails/right",
        "segmentedNails/right_unrotated"]
    dir_paths = []
    for directory in directories:
        dir_path = os.path.join(os.getcwd(), directory)
        dir_paths.append(dir_path)
        if not os.path.exists(directory):
            os.makedirs(dir_path)
        else:
            pass

# clean up directory
def clean_directories(dir_list):
    for directory in dir_list:
        shutil.rmtree(os.path.join(os.getcwd(), directory), ignore_errors = True)

# clean files
def clean_files(file_list):
    for file in file_list:
        filepath = os.path.join(os.getcwd(), file)
        if(os.path.exists(filepath)):
            os.remove(filepath)
        else:
            pass

def getTIPLandmarks(multi_hand_landmarks, img_shape):
    # input is results.multi_hand_landmarks
    # will focus on all fingers except thumb - i.e index, middle, ring and pinky
    for hand_landmarks in multi_hand_landmarks:
        index_finger_tip = [round(hand_landmarks.landmark[mp_hands.HandLandmark.INDEX_FINGER_TIP].x* img_shape[1]), round(hand_landmarks.landmark[mp_hands.HandLandmark.INDEX_FINGER_TIP].y* img_shape[0])]
        middle_finger_tip = [round(hand_landmarks.landmark[mp_hands.HandLandmark.MIDDLE_FINGER_TIP].x* img_shape[1]), round(hand_landmarks.landmark[mp_hands.HandLandmark.MIDDLE_FINGER_TIP].y* img_shape[0])]
        ring_finger_tip = [round(hand_landmarks.landmark[mp_hands.HandLandmark.RING_FINGER_TIP].x* img_shape[1]), round(hand_landmarks.landmark[mp_hands.HandLandmark.RING_FINGER_TIP].y* img_shape[0])]
        pinky_tip = [round(hand_landmarks.landmark[mp_hands.HandLandmark.PINKY_TIP].x* img_shape[1]), round(hand_landmarks.landmark[mp_hands.HandLandmark.PINKY_TIP].y* img_shape[0])]

    tips_dict = {'index': index_finger_tip, 'middle': middle_finger_tip, 'ring': ring_finger_tip, 'pinky': pinky_tip}
    return(tips_dict)            

def getDIPLandmarks(multi_hand_landmarks, img_shape):
    # input is results.multi_hand_landmarks
    # will focus on all fingers except thumb - i.e index, middle, ring and pinky
    for hand_landmarks in multi_hand_landmarks:
        index_finger_dip = [round(hand_landmarks.landmark[mp_hands.HandLandmark.INDEX_FINGER_DIP].x* img_shape[1]), round(hand_landmarks.landmark[mp_hands.HandLandmark.INDEX_FINGER_DIP].y* img_shape[0])]
        middle_finger_dip = [round(hand_landmarks.landmark[mp_hands.HandLandmark.MIDDLE_FINGER_DIP].x* img_shape[1]), round(hand_landmarks.landmark[mp_hands.HandLandmark.MIDDLE_FINGER_DIP].y* img_shape[0])]
        ring_finger_dip = [round(hand_landmarks.landmark[mp_hands.HandLandmark.RING_FINGER_DIP].x* img_shape[1]), round(hand_landmarks.landmark[mp_hands.HandLandmark.RING_FINGER_DIP].y* img_shape[0])]
        pinky_dip = [round(hand_landmarks.landmark[mp_hands.HandLandmark.PINKY_DIP].x* img_shape[1]), round(hand_landmarks.landmark[mp_hands.HandLandmark.PINKY_DIP].y* img_shape[0])]

    dips_dict = {'index': index_finger_dip, 'middle': middle_finger_dip, 'ring': ring_finger_dip, 'pinky': pinky_dip}
    return(dips_dict)            

def getTipsFromHull(tips_landmarks, hull):
    # input is TIPLandmarks and hull points
    # For each landmark we find the nearest hull point, i.e matching the hull points
    # with the tips, so that we can associate which sections of the contour belong 
    # to which finger
    
    # index finger
    index_dist = list(map(lambda x: np.linalg.norm(x-tips_landmarks['index']), hull[0]))
    index_hull_point = hull[0][index_dist.index(min(index_dist))][0]
    
    # middle finger
    middle_dist = list(map(lambda x: np.linalg.norm(x-tips_landmarks['middle']), hull[0]))
    middle_hull_point = hull[0][middle_dist.index(min(middle_dist))][0]

    # ring finger
    ring_dist = list(map(lambda x: np.linalg.norm(x-tips_landmarks['ring']), hull[0]))
    ring_hull_point = hull[0][ring_dist.index(min(ring_dist))][0]

    # pinky 
    pinky_dist = list(map(lambda x: np.linalg.norm(x-tips_landmarks['pinky']), hull[0]))
    pinky_hull_point = hull[0][pinky_dist.index(min(pinky_dist))][0]

    tips_hulls_dict = {'index': index_hull_point, 'middle': middle_hull_point, 'ring': ring_hull_point, 'pinky': pinky_hull_point}
    return(tips_hulls_dict)            

def getClosestPixelInHull(pixel_in, contour_section):
    # Given a pixel_in and contour_section, output the pixel in the contour_section that is closese to pixel_in
    index_dist = list(map(lambda x: np.linalg.norm(x-pixel_in), contour_section[0]))
    index_point = contour_section[0][index_dist.index(min(index_dist))][0]
    return(index_point)


def locatePixelInList(pixel,input_list):
    # Given a contour find the index of the input pixel (mostly one from the convex hull)
    
    temp_list = list([(input_list[0][x][0] == pixel).all() for x in range(len(input_list[0]))])
    # gives a list of true/false 
    pixel_index = temp_list.index(max(temp_list))
    # pick the true
    
    return(pixel_index)

def getHand(img, hand= 'left'):
    """
    Split the given two-hand image into a single hand image.
        
    :param img:   input RGB image
    :param hand:  'left' - left half of the picture 
                  'right' - right half of the image
        
    """
    rows, cols, channels = img.shape
    new_cols = round(cols/2)

    
    if hand == 'left':
        img_cropped = img[:, 0:new_cols-1,:]
    elif hand == 'right':
        img_cropped = img[:, new_cols:cols-1,:]
    else:
        print('Returning input image')
        img_cropped = img
    
    return img_cropped
    

def getBinaryImage(im, min_foreground_pixel = 10, max_foreground_pixel = 255):
    # Thresholds the given image(segmented mask with black background) to give a black-white binary image
    # with the background being white pixels and foreground(hand) being white pixels
    
    ### Convert to Gray Scale
    imgray = cv.cvtColor(im, cv.COLOR_BGR2GRAY)

    ### Binary Threshold
    ret, thresh = cv.threshold(imgray, min_foreground_pixel, max_foreground_pixel, cv.THRESH_BINARY)
    
    return(thresh)

def getContoursAndHull(black_white_im):
    # Given a binary(black and white image with segmented hand mask) give output of contours, hierarchy
    # and convex hull points

    # returns only the largest contour as this is the one we want for the hand!
    
    ### Find contours
    contours, hierarchy = cv.findContours(black_white_im, cv.RETR_TREE, cv.CHAIN_APPROX_NONE)
    
    ### contour lengths
    contour_lengths = [len(cc) for cc in contours]
    largest_contour_index = contour_lengths.index(max(contour_lengths))
    
    # subset contours and hierarchy to the largest contour
    contours = [contours[largest_contour_index]]
    hierarchy = np.array([[hierarchy[0][largest_contour_index]]])
    
    ### Convex Hull
    # create hull array for convex hull points
    hull = []
    # calculate points for each contour
    for i in range(len(contours)):
        # creating convex hull object for each contour
        hull.append(cv.convexHull(contours[i], False))
    
    return({'contours':contours, 'hierarchy':hierarchy, 'hull': hull})

def drawConvexHull(black_white_im, contours, hierarchy, hull, draw_Contour = True):
    # given a black white image, contours and hull, output a image with contours and hull drawn
    
    # create an empty black image
#     drawing = np.zeros((black_white_im.shape[0], black_white_im.shape[1], 3), np.uint8)
    
    drawing = black_white_im

    # draw contours and hull points
    color_contours = (0, 255, 0) # green - color for contours
    color = (255, 0, 0) # blue - color for convex hull
    
    # draw ith contour
    if draw_Contour:
        cv.drawContours(drawing, contours, -1, color_contours, 1, 8, hierarchy)
    
    # draw ith convex hull object
    cv.drawContours(drawing, hull, -1, color, 1, 8)
    
    return(drawing)

def cropImageFromContour(img, cnt):
    # Crop the image(img) based on the input of a closed contour(cnt), set of points
    # adapted from https://www.life2coding.com/cropping-polygon-or-non-rectangular-region-from-image-using-opencv-python/
    mask = np.zeros(img.shape[0:2], dtype=np.uint8)
    
    # draw the contours on the mask
    cv.drawContours(mask, [cnt], -1, (255, 255, 255), -1, cv.LINE_AA)
    
    res = cv.bitwise_and(img,img,mask = mask)
    rect = cv.boundingRect(cnt) # returns (x,y,w,h) of the rect
    cropped = res[rect[1]: rect[1] + rect[3], rect[0]: rect[0] + rect[2]]
    
#     ## To get white background cropped image
#     ## create the white background of the same size of original image
#     wbg = np.ones_like(img, np.uint8)*255
#     cv.bitwise_not(wbg,wbg, mask=mask)
    
#     # overlap the resulted cropped image on the white background
#     dst = wbg+res
#     dst_cropped = dst[rect[1]: rect[1] + rect[3], rect[0]: rect[0] + rect[2]]

    return(cropped)

def cropWarpedRect(img, rect):
    #### BEST
    
    # from https://stackoverflow.com/questions/11627362/how-to-straighten-a-rotated-rectangle-area-of-an-image-using-opencv-in-python/48553593#48553593
    # Get center, size, and angle from rect
    center, size, theta = rect
    
    box = cv.boxPoints(rect)
    box = np.int0(box)
    width = int(rect[1][0])
    height = int(rect[1][1])

    src_pts = box.astype("float32")
    dst_pts = np.array([[0, height-1],
                        [0, 0],
                        [width-1, 0],
                        [width-1, height-1]], dtype="float32")
    M = cv.getPerspectiveTransform(src_pts, dst_pts)
    warped = cv.warpPerspective(img, M, (width, height))
    if (theta > 45):
        warped = cv.rotate(warped, cv.ROTATE_90_CLOCKWISE)

    return(warped)

def getContourSubsectionPercentage(contour_in, pixel_in, percent = 5):
    # get subsection of contour that is 5%(default) left and right to the input pixel
    input_pixel_location = locatePixelInList(pixel_in, contour_in)  
    nPixel_contour = len(contour_in[0])
    
    section_left = int(max(0, input_pixel_location - round(nPixel_contour*percent/100)))
    section_right = int(min(nPixel_contour-1, input_pixel_location + round(nPixel_contour*percent/100)))  
    
    print(section_left)
    print(section_right)
    
    subContour = [np.array(contour_in[0][section_left:section_right])]
    
    return(subContour)

def getContourSubsection(contour_in, pixel_on_contour, ref_pixel):
    # Given a pixel on the contour, split the contour into two sections left of the pixel
    # and right of the pixel.
    # Then find the closest points in both sections (left and right) to the ref_pixel, and 
    # then give subset the contour between these two points
    # The idea is when we give a contour of the hand, and a point on it corresponding to the
    # finger tip, we get two sections of the contour, to the left of the finger enf then right
    # We then find the points closest to these 
    
    input_pixel_location = locatePixelInList(pixel_on_contour, contour_in)  
    nPixel_contour = len(contour_in[0])
    
    # roll/shift the array so that input_pixel is the middle of array
    contour_rolled = [np.array(np.roll(contour_in[0],2*(round(nPixel_contour/2)-input_pixel_location)))]
    
    section_left = [np.array(contour_rolled[0][0:round(nPixel_contour/2)])]
    section_right = [np.array(contour_rolled[0][round(nPixel_contour/2):nPixel_contour])]
    
    closest_pixel_left = getClosestPixelInHull(ref_pixel, section_left)
    closest_pixel_right = getClosestPixelInHull(ref_pixel, section_right)
    
    closest_pixel_left_location = locatePixelInList(closest_pixel_left, contour_rolled)
    closest_pixel_right_location = locatePixelInList(closest_pixel_right, contour_rolled)
    
    subContour = [np.array(contour_rolled[0][(closest_pixel_left_location-1):closest_pixel_right_location])]
    subContour = [np.array(np.roll(subContour[0],-2*(round(nPixel_contour/2)-input_pixel_location)))]
    
#     subContour = [np.array([[pixel_on_contour], [closest_pixel_left], [closest_pixel_right]])]
#     return({'left': closest_pixel_left, 'right': closest_pixel_right})
    return(subContour)



# Function to create required directory
def create_directories():
    directories = [
        "testHulls/left",
        "testHulls/right",
        "segmentedNails/left",
        "segmentedNails/left_unrotated",
        "segmentedNails/right",
        "segmentedNails/right_unrotated"]
    dir_paths = []
    for directory in directories:
        dir_path = os.path.join(os.getcwd(), directory)
        dir_paths.append(dir_path)
        if not os.path.exists(directory):
            os.makedirs(dir_path)
        else:
            pass
    return(dir_paths)

# Helperrr function to create mapping for table
def create_mapping(filepath_array, 
                   recordId_array, 
                   createdOn_array, 
                   participantId_array,
                   synId_array,
                  fingerKey_array):
    manifest_data = pd.DataFrame({
        "path": filepath_array,
        "recordId":recordId_array, 
        "createdOn":createdOn_array, 
        "participantId":participantId_array,
        "used":synId_array,
        "finger_key" : fingerKey_array
    })
    return(manifest_data)

def process_hand_images(data, 
                        target_directory,
                        target_nails_directory,
                        img_all_directory, 
                        which_hand):
    fail_index = 0
    pass_index = 0
    nails_rects = {} # save image name and rect dictionaries, i.e image vs nail bounding boxes
    filepath_array = []
    recordId_array = []
    createdOn_array = []
    participantId_array = []
    synId_array = []
    fingerKey_array = []
    for index in range(data.shape[0]):
        # append array
        curr_recordId = data["recordId"].iloc[index]
        curr_createdOn = data["createdOn"].iloc[index]
        curr_participantId = data["participantId"].iloc[index]
        curr_synId = data["syn_id"].iloc[index]
        
        # update path for current image
        current_image_path = img_all_directory + data["fuse"].iloc[index]
        current_orig_path = img_all_directory + data["name"].iloc[index]

        # update target path for the contoured image for the current image
        current_target_path = target_directory + '/' + which_hand + '/' + data["fuse"].iloc[index]

        # update target path for segmented nails from the current image (ROTATED)
        current_nails_target_path = target_nails_directory + '/' + which_hand

        # update target path for segmented nails from the current image (UN-ROTATED)
        current_nails_target_path_unrotated = target_nails_directory + '/' + which_hand + '_unrotated'

        # read images
        img = cv.imread(current_image_path)
        orig_img = cv.imread(current_orig_path)

        # Masks of left and right img (fuse files)
        img = getHand(img, which_hand)

        # Actual photos
        orig_img = getHand(orig_img, which_hand)

        # clones of actual images
        orig_img_clone = orig_img.copy()

        # resize image [for faster calculation, and mediapipe usually takes in small images of size
        # 300 x 300][https://github.com/google/mediapipe/blob/master/mediapipe/calculators/image/scale_image_calculator.cc]
        img_shape  = orig_img.shape
        resize_factor = round(300/max(img_shape),3) # resize max(length, width) to 300 pixels
        orig_img_resize = cv.resize(orig_img, None, fx = resize_factor , fy = resize_factor, interpolation = cv.INTER_AREA)

        color_contours = (0, 255, 0) # green - color for contours

        ### left hand
        bw_img = getBinaryImage(img)
        
        # resize the left hand mask to match that of the original image
        img_shape = orig_img.shape[0:2]
        bw_img = cv.resize(bw_img, (img_shape[1], img_shape[0]), interpolation = cv.INTER_AREA)

        ###
        contours_hull = getContoursAndHull(bw_img)

        ###
        dw_img = drawConvexHull(orig_img, contours_hull['contours'], contours_hull['hierarchy'], contours_hull['hull'])

        # apply mediapipe to get hand landmarks
        results = hands.process(cv.cvtColor(orig_img_resize, cv.COLOR_BGR2RGB))

        # to draw all landmarks from mediapipe
        if not results.multi_hand_landmarks:
            cv.imwrite(current_target_path, dw_img)
            nails_rects[data["name"].iloc[index]] = {}
            fail_index = fail_index + 1
        else:
            dw_img = drawConvexHull(orig_img, contours_hull['contours'], contours_hull['hierarchy'], contours_hull['hull'], draw_Contour=True)
            for hand_landmarks in results.multi_hand_landmarks:
                mp_drawing.draw_landmarks(dw_img, hand_landmarks, mp_hands.HAND_CONNECTIONS)

            # get tip landmarks from results
            tips_landmarks = getTIPLandmarks(results.multi_hand_landmarks, img.shape)

            # get DIP landmarks from results
            dips_landmarks = getDIPLandmarks(results.multi_hand_landmarks, img.shape)

            # get points closest to tips from hull
            tips_hull = getTipsFromHull(tips_landmarks, contours_hull['hull'])

            # get subcontours for each finger
            subContours = {}

            # get minimum bounding rectangle(min area) for each subContour
            subContourRects = {}
            subContourBoxes = {}

            for finger_key in tips_hull:
                subContours[finger_key] = getContourSubsection(contours_hull['contours'], tips_hull[finger_key], dips_landmarks[finger_key])
    #             subContours[finger_key] = getContourSubsectionPercentage(contours_hull['contours'], tips_hull[finger_key])
                rect = cv.minAreaRect(subContours[finger_key][0])
                box = cv.boxPoints(rect)
                box = np.int0(box)
                subContourRects[finger_key] = rect
                subContourBoxes[finger_key] = [box]

             # draw SubContours finger
            for finger_key in subContours:
                cv.drawContours(dw_img, subContours[finger_key], -1, (255,0,0), 1, 8, contours_hull['hierarchy'])
                cv.drawContours(dw_img, subContourBoxes[finger_key],-1,(0,0,255))

                # rotated via rects
                cropped_finger = cropWarpedRect(orig_img_clone, subContourRects[finger_key])
                curr_file_name = which_hand + '_' + finger_key + '_' + data["fuse"].iloc[index]
                curr_path = current_nails_target_path + '/' + curr_file_name
                if min(cropped_finger.shape) > 0 :
                    cv.imwrite(curr_path, cropped_finger)
                    # upload this corrected cropped finger to synapse via customSynpaseUpload
                    # customSynapseUpload(curr_path, curr_file_name)
                    filepath_array.append(curr_path)
                    recordId_array.append(curr_recordId)
                    createdOn_array.append(curr_createdOn)
                    participantId_array.append(curr_participantId)
                    synId_array.append(curr_synId)
                    fingerKey_array.append(which_hand + "_" + finger_key)

                # unrotated fingers
                cropped_finger_unrotated = cropImageFromContour(orig_img_clone, subContourBoxes[finger_key][0])
                curr_file_name = which_hand + '_' + finger_key + '_' + data["fuse"].iloc[index]
                curr_path = current_nails_target_path_unrotated + '/' + curr_file_name
                if min(cropped_finger_unrotated.shape) >0 :
                    cv.imwrite(curr_path, cropped_finger_unrotated)

            cv.imwrite(current_target_path, dw_img)
            nails_rects[data["name"].iloc[index]] = subContourRects
            pass_index = pass_index + 1

    print(which_hand + ' fail percent')
    print(100 * fail_index/(fail_index+pass_index))

    data_mapping = create_mapping(filepath_array, recordId_array, createdOn_array, 
                                  participantId_array, synId_array, fingerKey_array)
    
    result = {
        "mapping": data_mapping,
        "nails_rects": nails_rects
    }
    return(result)

# function to rerieve original mapping of the hand images
def get_original_image_mapping(folder_id):
    image_folder_obj = syn.getChildren(
        folder_id)
    data = pd.DataFrame(
        image_folder_obj)
    result_df = {}
    result_df["recordId"] = []
    result_df["createdOn"] = []
    result_df["participantId"] = []
    result_df["name"] = []
    result_df["syn_id"] = []
    for name, syn_id in zip(data["name"], data["id"]):
        kv_pair = syn.get_annotations(syn_id)
        result_df["recordId"].append(kv_pair["recordId"][0])
        result_df["createdOn"].append(kv_pair["createdOn"][0])
        result_df["participantId"].append(kv_pair["participantId"][0])
        result_df["name"].append(name)
        result_df["syn_id"].append(syn_id)

    result = pd.DataFrame(result_df)
    result = result[~result["name"].str.contains(pat = "leftHandImaging|rightHandImaging")]
    return(result)

# function to retrieve hand images with its mask  
def get_image_fuse_mapping(folder_id, output_loc):
    ## Getting hands images from Synapse
    # Hand mask images form slack (check June 16 - Chat with Dan Webster, Aryton Tediarjo and Meghasyam Tummalacherla)
    # Also in https://www.synapse.org/#!Synapse:syn25999658
    # Download the curated hand image files from Synapse
    hands_all_files = synapseutils.syncFromSynapse(
        syn = syn, 
        entity= folder_id, 
        path= output_loc)
    all_files = os.listdir(output_loc)
    fuse_files = list(filter(lambda x: 'fuse' in x, all_files))
    orig_files = list(map(lambda x: x[:-11], fuse_files)) # remove ___fuse.png
    target_directory = 'testHulls' # directory with mediapipe results
    target_nails_directory = 'segmentedNails' # directory with segmented nails
    fuse_mapping = pd.DataFrame({"name":orig_files, "fuse":fuse_files})
    return(fuse_mapping)

# Table
# -- Delete if exists alongside the schema
def create_column_obj():
    cols = [
        Column(name='recordId', columnType='STRING', maximumSize=50),
        Column(name='createdOn', columnType='DATE'),
        Column(name='participantId', columnType='STRING', maximumSize = 50),
        Column(name='finger_key', columnType='STRING', maximumSize=20),
        Column(name='finger_segments', columnType='FILEHANDLEID')]
    return(cols)


# create directories
create_directories()

# fetch images from Synapse and direct it into 
# the right directory
full_res_image_mapping = get_original_image_mapping(
    FULL_RES_IMAGE_MAPPING_FOLDER_ID)
fuse_image_mapping = get_image_fuse_mapping(
    FUSE_IMAGE_MAPPING_FOLDER_ID, 
    OUTPUT_DIRECTORY)

# # merge all mapping
image_mapping = pd.merge(
    full_res_image_mapping, 
    fuse_image_mapping, 
    on = "name", 
    how = "inner")

# # get fingers segmentation
left_fingers_data = process_hand_images(
    data = image_mapping,
    target_directory = TARGET_DIRECTORY,
    target_nails_directory = TARGET_NAILS_DIR,
    img_all_directory = OUTPUT_DIRECTORY,
    which_hand = "left")


right_fingers_data = process_hand_images(
    data = image_mapping,
    target_directory = TARGET_DIRECTORY,
    target_nails_directory = TARGET_NAILS_DIR,
    img_all_directory = OUTPUT_DIRECTORY, 
    which_hand = "right")

# concatenate table
table = pd.concat(
    [left_fingers_data["mapping"], 
    right_fingers_data["mapping"]])

# Sync Images to Folder Entity as File Entity
output_file = os.path.join(
    os.getcwd(), "manifest.tsv")
table["parent"] = SEGMENTED_NAIL_OUTPUT_ID
table.drop(["createdOn"], axis = 1).to_csv(
    output_file, sep = "\t", index = False)
synapseutils.syncToSynapse(
    syn = syn, 
    manifestFile = output_file,
    dryRun = False)

# Save rectangular bounding boxes data to Synapse as File Entity
# Minimum bounding rects of nails into a dataframe (left hand)
aa_left = pd.DataFrame.from_dict(left_fingers_data["nails_rects"],
                                 orient = 'index')
aa_left.columns = 'left_' + aa_left.columns
aa_left['image'] = aa_left.index
aa_left.index = range(len(aa_left.index))
aa_right = pd.DataFrame.from_dict(right_fingers_data["nails_rects"],
                                  orient = 'index')
aa_right.columns = 'right_' + aa_right.columns
aa_right['image'] = aa_right.index
aa_right.index = range(len(aa_right.index))
aa = pd.merge(aa_left, aa_right, on = 'image', how = 'outer')

### Upload nail bounding rects to Synapse
output_file = os.path.join(
    os.getcwd(), NAILS_RERCT_OUTPUT_FILE)
aa.to_csv(output_file, sep = '\t')

# Upload results to Synapse
activity = Activity(
    name='get nail bounding box',
    description='Minimum bounding rectangle for nails (except thumb). Each rectangle is of the form (center, size, theta)',
    used = FUSE_IMAGE_MAPPING_FOLDER_ID,
    executed = GIT_URL)
file = File(
    output_file,
    description='Minimum bounding rectangle for nails (except thumb). Each rectangle is of the form (center, size, theta)',
    parent=NAILS_RECT_OUTPUT_ID)
syn.store(file, activity = activity)

# upload as a table
table_exist = False
column_obj = create_column_obj()
schema = Schema(name=TABLE_NAME, columns=column_obj, parent=PARENT_ID)
for child in syn.getChildren(PARENT_ID):
    if(child["name"] == TABLE_NAME):
        table_exist = True
        results = syn.tableQuery("select * from %s" %child["id"])
        syn.delete(results)
    else:
        pass

# store schema
if not table_exist:
    syn.store(Schema(
        name = TABLE_NAME, 
        columns = schema, 
        parent = PARENT_ID))
else:
    pass

# upload filehandles
row_ids = []
for row in table["path"]:
    basename = os.path.basename(row)
    file_handle = syn.uploadFileHandle(row, PARENT_ID)
    row_ids.append(file_handle['id'])

# store tables
table["finger_segments"] = row_ids
table = table[["recordId", "createdOn", "participantId", "finger_key", "finger_segments"]]
syn.store(Table(schema, table))
    
# clean everything
clean_files(["manifest.tsv", NAILS_RERCT_OUTPUT_FILE])
clean_directories([TARGET_DIRECTORY, TARGET_NAILS_DIR, OUTPUT_DIRECTORY])