import numpy as np
import matplotlib.pyplot as plt
from setslope_v2 import set_fslope_v2
from Camo_stim import get_white_noise_image, make_target_mask, make_target_mask_centred, smooth_edges, smooth_edges_centred

BKG_slope = -1
Target_slope1 = -0.5

Target_slope2 = -3.5
im_height = 650
target_height = 120  # 90 OR 120
sigma = 0.5

my_dpi = 120 # to adapt to each computer

newMEAN = 128
newSTD = 32

cond = 'solo'  # '2AFC' or 'solo'
BKG = 'noise'  # 'gray' or 'noise'

for i in range(1, N):  # N = number of images to create
    print(i)
    if BKG == 'noise':
        if cond == 'solo':
            whiteN_BKG = get_white_noise_image(im_height, im_height)
            whiteN_T = get_white_noise_image(im_height, im_height)

            sloppyBKG = set_fslope_v2(img=whiteN_BKG, tgt_slope=BKG_slope)  # , center_range=5)
            sloppytarget = set_fslope_v2(img=whiteN_T, tgt_slope=Target_slope1)  # , center_range=5)
            sloppyBKG = np.subtract(sloppyBKG, 1)
            sloppytarget = np.subtract(sloppytarget, 1)

            target_mask, centreX, centreY = make_target_mask(im_height=im_height, target_height=target_height)
            centreY = centreY[0, ]
            centreX = centreX[0, ]
            
            # Normalisation: luminance matching
            lumMatchSlopeBKG = ((sloppyBKG - np.mean(sloppyBKG.flatten())) / np.std(
                sloppyBKG.flatten())) * newSTD + newMEAN
            lumMatchSlopeTarget = ((sloppytarget - np.mean(sloppytarget.flatten())) / np.std(
                sloppytarget.flatten())) * newSTD + newMEAN
            
            Target_filter, BKG_filter = smooth_edges(sigma, im_height, target_height, target_mask)
            filteredTarget = np.multiply(lumMatchSlopeTarget, Target_filter)
            filteredBKG = np.multiply(lumMatchSlopeBKG, BKG_filter)
            masked_data_filtered = np.add(filteredBKG, filteredTarget)
            
            figname = f'Fig{BKG_slope}BKG{Target_slope1}target_centreXY_{centreX}_{centreY}_Blur{sigma}_GS_{i}.png'

            plt.figure(figsize=(845 / my_dpi, 845 / my_dpi), dpi=my_dpi)
            plt.imshow(masked_data_filtered, cmap='gray')
            plt.axis('off')
            plt.savefig(figname, bbox_inches='tight', pad_inches=0, dpi=my_dpi)
            plt.show(block=False)

            plt.close()

        elif cond == '2AFC':
            whiteN_BKG = get_white_noise_image(im_height, im_height)
            whiteN_T1 = get_white_noise_image(im_height, im_height)
            whiteN_T2 = get_white_noise_image(im_height, im_height)

            sloppyBKG = set_fslope_v2(img=whiteN_BKG, tgt_slope=BKG_slope)  # , center_range=5)
            sloppytarget_1 = set_fslope_v2(img=whiteN_T1, tgt_slope=Target_slope1)  # , center_range=5)
            sloppytarget_2 = set_fslope_v2(img=whiteN_T2, tgt_slope=Target_slope2)  # , center_range=5)
            sloppyBKG = np.subtract(sloppyBKG, 1)
            sloppytarget_1 = np.subtract(sloppytarget_1, 1)
            sloppytarget_2 = np.subtract(sloppytarget_2, 1)

            target_mask = make_target_mask_centred(im_height=im_height, target_height=target_height)

            # Normalisation: lumMatch
            lumMatchSlopeBKG = ((sloppyBKG - np.mean(sloppyBKG.flatten())) / np.std(sloppyBKG.flatten())) * newSTD + newMEAN
            lumMatchSlopeTarget_1 = ((sloppytarget_1 - np.mean(sloppytarget_1.flatten())) / np.std(sloppytarget_1.flatten())) * newSTD + newMEAN
            lumMatchSlopeTarget_2 = ((sloppytarget_2 - np.mean(sloppytarget_2.flatten())) / np.std(sloppytarget_2.flatten())) * newSTD + newMEAN

            sloppytargetS = np.append(lumMatchSlopeTarget_1, lumMatchSlopeTarget_2, axis=1)
            stim_fusedT = np.multiply(sloppytargetS, target_mask)

            Target_filter, BKG_filter = smooth_edges_centred(sigma, im_height, target_height, target_mask)
            filteredTarget = np.multiply(sloppytargetS, Target_filter)
            filteredBKG = np.multiply(np.append(lumMatchSlopeBKG, lumMatchSlopeBKG, axis=1), BKG_filter)  # lnormSlopeBKG // lumMatchSlopeBKG

            masked_data_filtered = np.add(filteredBKG, filteredTarget)

            circle1 = plt.Circle((im_height / 2, im_height / 2), radius=(target_height / 2) + 1, linewidth=1.5,
                                 edgecolor=[0.5, 0.5, 0.5], facecolor='none')
            circle2 = plt.Circle((im_height * 1.5, im_height / 2), radius=(target_height / 2) + 1, linewidth=1.5,
                                 edgecolor=[0.5, 0.5, 0.5], facecolor='none')

            figname = f'LumMatch_Fig{BKG_slope}BKG{Target_slope1}vs{Target_slope2}-target{target_height}_Blur{sigma}_lwidth1.5_GS_{i}.png'
            
            fig, ax = plt.subplots()
            ax.imshow(masked_data_filtered, cmap='gray')
            ax.add_patch(circle1)
            ax.add_patch(circle2)
            plt.axis('off')
            plt.savefig(figname, dpi=300, bbox_inches='tight', pad_inches=0)
            plt.show(block=False)

            plt.close()

    elif BKG == 'gray':
      GrayBKG = np.ones((im_height, im_height * 2))
      whiteN_T1 = get_white_noise_image(im_height, im_height)
      whiteN_T2 = get_white_noise_image(im_height, im_height)
    
      sloppytarget_1 = set_fslope_v2(img=whiteN_T1, tgt_slope=Target_slope1)  # , center_range=5)
      sloppytarget_2 = set_fslope_v2(img=whiteN_T2, tgt_slope=Target_slope2)  # , center_range=5)
      sloppytarget_1 = np.subtract(sloppytarget_1, 1)
      sloppytarget_2 = np.subtract(sloppytarget_2, 1)
     
    # Normalisation: lumMatch
      lumMatchSlopeTarget_1 = ((sloppytarget_1 - np.mean(sloppytarget_1.flatten())) / np.std(
          sloppytarget_1.flatten())) * newSTD + newMEAN
      lumMatchSlopeTarget_2 = ((sloppytarget_2 - np.mean(sloppytarget_2.flatten())) / np.std(
          sloppytarget_2.flatten())) * newSTD + newMEAN 

      target_mask = make_target_mask_centred(im_height=im_height, target_height=target_height)
      sloppytargetS = np.append(lumMatchSlopeTarget_1, lumMatchSlopeTarget_2, axis=1)
      stim_fusedT = np.multiply(sloppytargetS, target_mask)
    
      GrayBKG = GrayBKG * 128
    
      masked_data = np.ma.masked_where(stim_fusedT > 0, GrayBKG, copy=True)
      print(np.min(stim_fusedT), np.max(stim_fusedT))
    
      figname = f'LumMatch_Fig_GrayBKG{Target_slope1}vs{Target_slope2}-target{target_height}_noBlur_GS_{i}.png'
      circle1 = plt.Circle((im_height / 2, im_height / 2), radius=(target_height / 2) + 0.5, linewidth=3,
                           edgecolor=[0.5, 0.5, 0.5], facecolor='none')
      circle2 = plt.Circle((im_height * 1.5, im_height / 2), radius=(target_height / 2) + 0.5, linewidth=3,
                           edgecolor=[0.5, 0.5, 0.5], facecolor='none')
    
      fig, ax = plt.subplots()
      ax.imshow(stim_fusedT, cmap='gray')
      ax.imshow(masked_data, cmap='gray', vmin=127, vmax=129)
      ax.add_patch(circle1)
      ax.add_patch(circle2)
      plt.axis('off')
      # plt.title('BKG: -2, target: -1, GS only')
      plt.savefig(figname, dpi=300, bbox_inches='tight', pad_inches=0)
      plt.show(block=False)  # block=False
    
      plt.close()

