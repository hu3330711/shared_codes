pro make_idl_aacgm

  cd,'.',current=dir

  files=['idl_aacgm','idl_aacgm_call','aacgm','altitude_to_cgm','cgm_to_altitude','coeff',$
    'convert_geo_coord','eqn_of_time','math','mlt','mlt1','rylm','solar_loc']

  make_dll,files,'IDL_Load',compile_directory=dir+'/src',input_directory=dir+'/src', $
    output_directory=dir

return
end
