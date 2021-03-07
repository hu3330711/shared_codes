;
;  $LastChangedBy: pulupa $
;  $LastChangedDate: 2019-08-01 13:39:30 -0700 (Thu, 01 Aug 2019) $
;  $LastChangedRevision: 27530 $
;  $URL: svn+ssh://thmsvn@ambrosia.ssl.berkeley.edu/repos/spdsoft/tags/spedas_4_1/projects/SPP/fields/util/spp_get_fft_bins_04_ac.pro $
;

function SPP_Get_FFT_bins_04_AC, bins


  if bins EQ 96 then begin

    temp=[0,146.48438,$
      146.48438,292.96876,$
      292.96875,439.45313,$
      439.45312,585.9375,$
      585.9375, 732.42188,$
      732.42188,878.90626,$
      878.90625,1025.39063,$
      1025.3906,1171.87498,$
      1171.875, 1318.35938,$
      1318.3594,1464.84378,$
      1464.8438,1611.32818,$
      1611.3281,1757.81248,$
      1757.8125,1904.29688,$
      1904.2969,2050.78128,$
      2050.7812,2197.26558,$
      2197.2656,2343.74998,$
      2343.75,2490.23438,$
      2490.2344,2636.71878,$
      2636.7188,2783.20318,$
      2783.2031,2929.68748,$
      2929.6875,3076.17188,$
      3076.1719,3222.65628,$
      3222.6562,3369.14058,$
      3369.1406,3515.62498,$
      3515.625, 3662.10938,$
      3662.1094,3808.59378,$
      3808.5938,3955.07818,$
      3955.0781,4101.56248,$
      4101.5625,4248.04688,$
      4248.0469,4394.53128,$
      4394.5312,4541.01558,$
      4541.0156,4687.49998,$
      4687.5, 4980.46875,$
      4980.4688,5273.43755,$
      5273.4375,5566.40625,$
      5566.4062,5859.37495,$
      5859.375, 6152.34375,$
      6152.3438,6445.31255,$
      6445.3125,6738.28125,$
      6738.2812,7031.24995,$
      7031.25,7324.21875,$
      7324.2188,7617.18755,$
      7617.1875,7910.15625,$
      7910.1562,8203.12495,$
      8203.125, 8496.09375,$
      8496.0938,8789.06255,$
      8789.0625,9082.03125,$
      9082.0312,9374.99995,$
      9375, 9960.9375,$
      9960.9375,10546.875,$
      10546.875,11132.8125,$
      11132.812,11718.7495,$
      11718.75, 12304.6875,$
      12304.688,12890.6255,$
      12890.625,13476.5625,$
      13476.562,14062.4995,$
      14062.5,14648.4375,$
      14648.438,15234.3755,$
      15234.375,15820.3125,$
      15820.312,16406.2495,$
      16406.25, 16992.1875,$
      16992.188,17578.1255,$
      17578.125,18164.0625,$
      18164.062,18749.9995,$
      18750,19921.875,$
      19921.875,21093.75,$
      21093.75, 22265.625,$
      22265.625,23437.5,$
      23437.5,24609.375,$
      24609.375,25781.25,$
      25781.25, 26953.125,$
      26953.125,28125,$
      28125,29296.875,$
      29296.875,30468.75,$
      30468.75, 31640.625,$
      31640.625,32812.5,$
      32812.5,33984.375,$
      33984.375,35156.25,$
      35156.25, 36328.125,$
      36328.125,37500,$
      37500,39843.75,$
      39843.75, 42187.5,$
      42187.5,44531.25,$
      44531.25, 46875,$
      46875,49218.75,$
      49218.75, 51562.5,$
      51562.5,53906.25,$
      53906.25, 56250,$
      56250,58593.75,$
      58593.75, 60937.5,$
      60937.5,63281.25,$
      63281.25, 65625,$
      65625,67968.75,$
      67968.75, 70312.5,$
      70312.5,72656.25,$
      72656.25, 75000]

    freq_lo = temp[0:191:2]
    freq_hi = temp[1:191:2]
    freq_avg= (freq_lo + freq_hi) / 2d

  endif




  if bins EQ 56 then begin

    temp = [0, 146.48438,$
      146.48438, 292.96876,$
      292.96875, 439.45313,$
      439.45312, 585.9375,$
      585.9375, 732.42188,$
      732.42188, 878.90626,$
      878.90625, 1025.39063,$
      1025.3906, 1171.87498,$
      1171.875, 1318.35938,$
      1318.3594, 1464.84378,$
      1464.8438, 1611.32818,$
      1611.3281, 1757.81248,$
      1757.8125, 1904.29688,$
      1904.2969, 2050.78128,$
      2050.7812, 2197.26558,$
      2197.2656, 2343.74998,$
      2343.75, 2636.71875,$
      2636.7188, 2929.68755,$
      2929.6875, 3222.65625,$
      3222.6562, 3515.62495,$
      3515.625, 3808.59375,$
      3808.5938, 4101.56255,$
      4101.5625, 4394.53125,$
      4394.5312, 4687.49995,$
      4687.5, 5273.4375,$
      5273.4375, 5859.375,$
      5859.375, 6445.3125,$
      6445.3125, 7031.25,$
      7031.25, 7617.1875,$
      7617.1875, 8203.125,$
      8203.125, 8789.0625,$
      8789.0625, 9375,$
      9375, 10546.875,$
      10546.875, 11718.75,$
      11718.75, 12890.625,$
      12890.625, 14062.5,$
      14062.5, 15234.375,$
      15234.375, 16406.25,$
      16406.25, 17578.125,$
      17578.125, 18750,$
      18750, 21093.75,$
      21093.75, 23437.5,$
      23437.5, 25781.25,$
      25781.25, 28125,$
      28125, 30468.75,$
      30468.75, 32812.5,$
      32812.5, 35156.25,$
      35156.25, 37500,$
      37500, 42187.5,$
      42187.5, 46875,$
      46875, 51562.5,$
      51562.5, 56250,$
      56250, 60937.5,$
      60937.5, 65625,$
      65625, 70312.5,$
      70312.5, 75000 ]


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
