python3 -m venv /root/env\
    && . /root/env/bin/activate \
    && python3 -m pip install --upgrade pip\
    && python3 -m pip install synapseclient\
    && python3 -m pip install opencv-python\
    && python3 -m pip install imageio