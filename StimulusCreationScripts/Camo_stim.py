"""
This code requires the setslope_v2.py code from Samuel Hulse.
1. White noise image
2. Target mask
3. Applying a Gaussian filter to smooth the target edges
"""

import matplotlib.pyplot as plt
from PIL import Image
import numpy as np
import cv2
from scipy.ndimage import gaussian_filter
from skimage import filters


# 1. White noise image


def get_white_noise_image(w, h):  # width and height
    pil_map = Image.fromarray(np.random.randint(0, 2, (w, h), dtype=np.dtype('uint8')))
    return pil_map


# 2. Target mask


def make_target_mask(im_height, target_height):
    x_sample = np.linspace(0, im_height, im_height)  # shape = (650, 650)
    columnsInImage, rowsInImage = np.meshgrid(x_sample, x_sample)
    centerX = np.random.randint(0 + target_height, im_height - target_height, 1)
    centerY = np.random.randint(0 + target_height, im_height - target_height, 1)
    radius = target_height / 2
    print('target_centerX: %.0f' % centerX, 'target_centerY: %.0f' % centerY, 'target_radius: %.0f' % radius)
    Xdisk = np.power((rowsInImage - centerY), 2)
    Ydisk = np.power((columnsInImage - centerX), 2)
    Rdisk = np.power(radius, 2)
    disk_mask = np.zeros((im_height, im_height), dtype=int)
    # print(disk_mask.shape)
    # x^2 + y^2 <= r^2  (0 < r < R)
    Dmatrix = Xdisk + Ydisk
    disk_mask[Dmatrix <= Rdisk] = 1

    return disk_mask, centerX, centerY


def make_target_mask_centred(im_height, target_height):
    x_sample = np.linspace(0, im_height, im_height)  # shape = (650, 650)
    y_sample = np.linspace(0, im_height, im_height)  # shape = (650, 650)
    columnsInImage, rowsInImage = np.meshgrid(y_sample, x_sample)
    centreY1 = im_height / 2
    centreX = im_height / 2
    radius = target_height / 2
    # print('target_centerY1: %.0f' % centreY1, 'target_centerX: %.0f' % centreX, 'target_radius: %.0f' % radius)
    Ydisk1 = np.power((columnsInImage - centreY1), 2)
    Ydisk2 = np.power((columnsInImage - centreY1), 2)
    Xdisk = np.power((rowsInImage - centreX), 2)  # rows: 0 : 650
    Rdisk = np.power(radius, 2)
    disk_mask = np.zeros((im_height, im_height * 2), dtype=int)
    # print(Ydisk1.shape, Ydisk2.shape, Xdisk.shape, disk_mask.shape)
    # x^2 + y^2 <= r^2  (0 < r < R)
    Dmatrix = np.append((Ydisk1 + Xdisk), (Ydisk2 + Xdisk), axis=1)
    # print(Dmatrix.shape)
    disk_mask[Dmatrix <= Rdisk] = 1

    return disk_mask


# 3. Applying a Gaussian filter to smooth the target edges


def smooth_edges(sigma, im_height, target_height, mask):
    filter_img = np.zeros((im_height, im_height), dtype=int)
    disk_mask = mask
    # disk_mask = make_target_mask(im_height, target_height)
    filter_img[disk_mask == 1] = 1
    Target_filter = filters.gaussian(filter_img, sigma=sigma)
    Target_filter = np.divide((np.subtract(Target_filter, np.min(Target_filter))),
                              (np.max(Target_filter) - np.min(Target_filter)))
    # print(np.shape(Target_filter), np.min(Target_filter), np.max(Target_filter))
    # plt.figure()
    # plt.imshow(Target_filter)
    # plt.title('Target_filter')
    # plt.show()
    # Target_filter = np.subtract(Gauss_filter, np.min(np.min(Gauss_filter)))
    # Target_filter = np.divide(Target_filter, (np.max(np.max(Target_filter))))
    BKG_filter = -Target_filter + 1
    # print(Target_filter, BKG_filter)
    return Target_filter, BKG_filter


def smooth_edges_centred(sigma, im_height, target_height, mask):
    filter_img = np.zeros((im_height, im_height*2), dtype=int)
    disk_mask = mask
    filter_img[disk_mask == 1] = 1
    Target_filter = filters.gaussian(filter_img, sigma=sigma)
    Target_filter = np.divide((np.subtract(Target_filter, np.min(Target_filter))),
                              (np.max(Target_filter) - np.min(Target_filter)))
    BKG_filter = -Target_filter + 1
    # Target_filter = np.append(Target_filter, Target_filter, axis=1)
    # BKG_filter = np.append(BKG_filter, BKG_filter, axis=1)

    return Target_filter, BKG_filter