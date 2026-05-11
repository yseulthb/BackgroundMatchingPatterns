"""
Basically how it works is to calculate the difference between the idealized image amplitude spectrum with the
image's original slope and the image itself, and then add those residuals to the new amplitude spectrum

Script written by S. Hulse - https://github.com/svhulse/Fourier-Analysis
"""

import numpy as np
import cv2
import os

from numpy.fft import fft2, ifft2, fftshift


def rotavg(data):
    center = np.divide(data.shape, 2)
    x_sample = np.linspace(0, data.shape[0], data.shape[0])
    y_sample = np.linspace(0, data.shape[1], data.shape[1])
    x, y = np.meshgrid(y_sample, x_sample)

    x = np.absolute(x - center[1])
    y = np.absolute(y - center[0])
    dist_matrix = np.sqrt(x ** 2 + y ** 2)

    max_dist = np.sqrt(np.sum(np.square(center)))
    n_bins = int(np.ceil(max_dist))
    bins = np.linspace(0, max_dist, n_bins)

    radialprofile = np.zeros(n_bins - 1)

    for i in range(len(bins[0:-1])):
        filter = np.zeros(data.shape)
        filter[np.logical_and(
            dist_matrix >= bins[i],
            dist_matrix < bins[i + 1])] = 1

        radialprofile[i] = np.sum(filter * data) / np.sum(filter)

    return radialprofile


def set_fslope_v2(img, tgt_slope):
    # Convert the image to the Fourier domain and normalize its Fourier transform by its greatest absolute value
    imgfft = fftshift(fft2(img))
    imgfft = imgfft / np.max(np.absolute(imgfft))

    # Generate a euclidean distance matrix matching the shape of the image
    center = np.divide(imgfft.shape, 2)
    x_sample = np.linspace(0, imgfft.shape[0], imgfft.shape[0])
    y_sample = np.linspace(0, imgfft.shape[1], imgfft.shape[1])
    x, y = np.meshgrid(y_sample, x_sample)

    x = x - center[1]
    y = y - center[0]
    dist_matrix = np.sqrt(x ** 2 + y ** 2) + 1

    # Separate the image into a phase matrix and an amplitude matrix
    F_p = imgfft / np.absolute(imgfft)
    F_a = np.absolute(imgfft) ** 2

    # Compute the slope of the image sfps
    r_avg = rotavg(F_a)
    x = np.linspace(1, len(r_avg), len(r_avg))

    slope = np.polyfit(np.log(x), np.log(r_avg), 1)[0]

    # Generate a residual matrix from the difference between the image and an even amplitude spectrum with the input
    # slope
    resid = np.log(F_a) - (slope * np.log(dist_matrix))

    # Apply the residuals to an amplitude matrix with the desired slope and apply this to the phase matrix
    tgt_matrix = tgt_slope * np.log(dist_matrix)
    output_amp = np.sqrt(np.exp(tgt_matrix + resid))
    output = np.multiply(F_p, output_amp)
    imout = np.absolute(ifft2(output))

    imout = imout / np.max(imout)
    imout = imout * 256  # why 256? check with Sam and try with 255 instead

    return imout


def get_sfps(img):
    imfft = fftshift(fft2(img))
    impfft = np.absolute(imfft) ** 2
    r_avg = rotavg(impfft)

    x = np.linspace(1, len(r_avg), len(r_avg))
    slope = np.polyfit(np.log(x), np.log(r_avg), 1)[0]

    return slope
