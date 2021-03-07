;
;  $LastChangedBy: pulupa $
;  $LastChangedDate: 2019-08-01 13:39:30 -0700 (Thu, 01 Aug 2019) $
;  $LastChangedRevision: 27530 $
;  $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_1/projects/SPP/fields/util/spp_get_fft_bins_04_dc.pro $
;

function SPP_Get_FFT_bins_04_DC, bins

  if bins EQ 96 then begin

    temp = [0, 9.1552734, $
      9.1552734, 18.3105468, $
      18.310547, 27.4658204, $
      27.46582,  36.6210934, $
      36.621094, 45.7763674, $
      45.776367, 54.9316404, $
      54.931641, 64.0869144, $
      64.086914, 73.2421874, $
      73.242188, 82.3974614, $
      82.397461, 91.5527344, $
      91.552734, 100.7080074, $
      100.70801, 109.8632834, $
      109.86328, 119.0185534, $
      119.01855, 128.1738234, $
      128.17383, 137.3291034, $
      137.3291,  146.4843734, $
      146.48438, 155.6396534, $
      155.63965, 164.7949234, $
      164.79492, 173.9501934, $
      173.9502,  183.1054734, $
      183.10547, 192.2607434, $
      192.26074, 201.4160134, $
      201.41602, 210.5712934, $
      210.57129, 219.7265634, $
      219.72656, 228.8818334, $
      228.88184, 238.0371134, $
      238.03711, 247.1923834, $
      247.19238, 256.3476534, $
      256.34766, 265.5029334, $
      265.50293, 274.6582034, $
      274.6582,  283.8134734, $
      283.81348, 292.9687534, $
      292.96875, 311.279297, $
      311.2793,  329.589847, $
      329.58984, 347.900387, $
      347.90039, 366.210937, $
      366.21094, 384.521487, $
      384.52148, 402.832027, $
      402.83203, 421.142577, $
      421.14258, 439.453127, $
      439.45312, 457.763667, $
      457.76367, 476.074217, $
      476.07422, 494.384767, $
      494.38477, 512.695317, $
      512.69531, 531.005857, $
      531.00586, 549.316407, $
      549.31641, 567.626957, $
      567.62695, 585.937497, $
      585.9375 , 622.558594, $
      622.55859, 659.179684, $
      659.17969, 695.800784, $
      695.80078, 732.421874, $
      732.42188, 769.042974, $
      769.04297, 805.664064, $
      805.66406, 842.285154, $
      842.28516, 878.906254, $
      878.90625, 915.527344, $
      915.52734, 952.148434, $
      952.14844, 988.769534, $
      988.76953, 1025.390624, $
      1025.3906, 1062.011694, $
      1062.0117, 1098.632794, $
      1098.6328, 1135.253894, $
      1135.2539, 1171.874994, $
      1171.875 , 1245.117188, $
      1245.1172, 1318.359388, $
      1318.3594, 1391.601588, $
      1391.6016, 1464.843788, $
      1464.8438, 1538.085988, $
      1538.0859, 1611.328088, $
      1611.3281, 1684.570288, $
      1684.5703, 1757.812488, $
      1757.8125, 1831.054688, $
      1831.0547, 1904.296888, $
      1904.2969, 1977.539088, $
      1977.5391, 2050.781288, $
      2050.7812, 2124.023388, $
      2124.0234, 2197.265588, $
      2197.2656, 2270.507788, $
      2270.5078, 2343.749988, $
      2343.75, 2490.23438, $
      2490.2344, 2636.71878, $
      2636.7188, 2783.20318, $
      2783.2031, 2929.68748, $
      2929.6875, 3076.17188, $
      3076.1719, 3222.65628, $
      3222.6562, 3369.14058, $
      3369.1406, 3515.62498, $
      3515.625,  3662.10938, $
      3662.1094, 3808.59378, $
      3808.5938, 3955.07818, $
      3955.0781, 4101.56248, $
      4101.5625, 4248.04688, $
      4248.0469, 4394.53128, $
      4394.5312, 4541.01558, $
      4541.0156, 4687.49998]

    freq_lo = temp[0:191:2]
    freq_hi = temp[1:191:2]
    freq_avg= (freq_lo + freq_hi) / 2d

  endif




  if bins EQ 56 then begin

    temp = [0, 9.1552734, $
      9.1552734, 18.3105468, $
      18.310547, 27.4658204, $
      27.46582,  36.6210934, $
      36.621094, 45.7763674, $
      45.776367, 54.9316404, $
      54.931641, 64.0869144, $
      64.086914, 73.2421874, $
      73.242188, 82.3974614, $
      82.397461, 91.5527344, $
      91.552734, 100.7080074, $
      100.70801, 109.8632834, $
      109.86328, 119.0185534, $
      119.01855, 128.1738234, $
      128.17383, 137.3291034, $
      137.3291, 146.4843734, $
      146.48438, 164.794927, $
      164.79492, 183.105467, $
      183.10547, 201.416017, $
      201.41602, 219.726567, $
      219.72656, 238.037107, $
      238.03711, 256.347657, $
      256.34766, 274.658207, $
      274.6582, 292.968747, $
      292.96875, 329.589844, $
      329.58984, 366.210934, $
      366.21094, 402.832034, $
      402.83203, 439.453124, $
      439.45312, 476.074214, $
      476.07422, 512.695314, $
      512.69531, 549.316404, $
      549.31641, 585.937504, $
      585.9375, 659.179688, $
      659.17969, 732.421878, $
      732.42188, 805.664068, $
      805.66406, 878.906248, $
      878.90625, 952.148438, $
      952.14844, 1025.390628, $
      1025.3906, 1098.632788, $
      1098.6328, 1171.874988, $
      1171.875, 1318.35938, $
      1318.3594, 1464.84378, $
      1464.8438, 1611.32818, $
      1611.3281, 1757.81248, $
      1757.8125, 1904.29688, $
      1904.2969, 2050.78128, $
      2050.7812, 2197.26558, $
      2197.2656, 2343.74998, $
      2343.75, 2636.71875, $
      2636.7188, 2929.68755, $
      2929.6875, 3222.65625, $
      3222.6562, 3515.62495, $
      3515.625, 3808.59375, $
      3808.5938, 4101.56255, $
      4101.5625, 4394.53125, $
      4394.5312, 4687.49995 ]


    freq_lo = temp[0:111:2]
    freq_hi = temp[1:111:2]
    freq_avg= (freq_lo + freq_hi) / 2d

  endif


  return_struct = {freq_lo:freq_lo, $
    freq_hi:freq_hi, $
    freq_avg:freq_avg}


  return, return_struct


  ;;THE
END

