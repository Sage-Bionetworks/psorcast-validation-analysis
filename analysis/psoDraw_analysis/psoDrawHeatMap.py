'''
This script is used to generate the heatmap for psoriasis draw
it will sync all images in synapse, parse it into dataframe
and merge each images into a heatmap by adding/normalizing the tensors

@author: dan.webster@sagebase.org, meghasyam@sagebase.org, aryton.tediarjo@sagebase.org
'''
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import imageio
import cv2
import synapseclient as sc
import synapseutils as su
from synapseclient import File, Activity

# login to synapse
syn = sc.login()

# Global Variables
PSORIASIS_DRAW_IMAGES = "syn25837658"
FIGURE_NAME = 'psoDrawHeatMap.png'
PARENT_ID = 'syn23632500'
GIT_URL = "https://github.com/Sage-Bionetworks/psorcast-validation-analysis/blob/main/analysis/psoDraw_analysis/psoDrawHeatMap.py"


# Helper Function
def color_histogram(image):
    color = ('b', 'g', 'r')
    for i, col in enumerate(color):
        histr = cv2.calcHist([image], [i], None, [256], [0, 256])
        plt.plot(histr, color=col)
        plt.xlim([0, 256])
    plt.show()


# isolate the pixels that signal drawing on the body and change to 1.0; all else 0.0
def isolate_psoriasis_red(image):
    width, height = image.shape[:2]
    image = image.copy()
    for x in range(width):
        for y in range(height):
            blue = image[x, y, 0]
            green = image[x, y, 1]
            red = image[x, y, 2]
            if (red > 100 and red < 150):
                image[x, y, 0] = 1.0
                image[x, y, 1] = 1.0
                image[x, y, 2] = 1.0
            else:
                image[x, y, 0] = 0.0
                image[x, y, 1] = 0.0
                image[x, y, 2] = 0.0
    return image


def crop_image_only_outside(img, tol=250):
    # img is 2D or 3D image data
    # tol  is tolerance
    mask = img < tol
    if img.ndim == 3:
        mask = mask.all(2)
    m, n = mask.shape
    mask0, mask1 = mask.any(0), mask.any(1)
    col_start, col_end = mask0.argmax(), n - mask0[::-1].argmax()
    row_start, row_end = mask1.argmax(), m - mask1[::-1].argmax()
    return img[row_start:row_end, col_start:col_end]


def sync_psoDraw_images():
    images = {}
    images["filename"] = []
    images["synId"] = []
    images["filepath"] = []
    for f in su.syncFromSynapse(syn, PSORIASIS_DRAW_IMAGES):
        images["filename"].append(f["name"])
        images["synId"].append(f["id"])
        images["filepath"].append(f["path"])
    images_df = pd.DataFrame(images)
    return images_df


def generate_psoDraw_heatmap(image_paths):
    pics = []
    for im_path in image_paths:
        im = imageio.imread(im_path)
        if len(im.shape) == 2:
            # pass by any pics without psoriatic plaques (0% BSA), these have no BGR value after conversion to jpg
            continue
        imCrop = crop_image_only_outside(im)
        fixedImage = cv2.resize(imCrop, (775, 775))
        justPlaquePixels = isolate_psoriasis_red(fixedImage)
        pics.append(justPlaquePixels[:, :, 1])
    output = sum(pics)
    # what percentage of the cohort has psoriasis in a given pixel
    normed = output[:, :] / (len(pics))
    width, height = normed.shape[:2]
    z_min, z_max = np.min(normed), np.max(normed)
    fig, ax = plt.subplots()
    c = ax.pcolormesh(normed, cmap='hot', vmin=z_min, vmax=z_max)
    ax.axis([0, 775, 775, 0])
    fig.colorbar(c, ax=ax)
    plt.axis('off')
    fig.savefig(FIGURE_NAME, dpi=300)


def main():
    # generate image
    images_df = sync_psoDraw_images()
    generate_psoDraw_heatmap(images_df["filepath"])

    # save to synapse
    activity = Activity(
        "generate psoDraw heatmap",
        used=PSORIASIS_DRAW_IMAGES,
        executed=GIT_URL
    )

    file_entity = File(
        FIGURE_NAME,
        parent=PARENT_ID)
    syn.store(file_entity, activity=activity)
    os.remove(FIGURE_NAME)


if __name__ == '__main__':
    main()
