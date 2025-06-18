add_library(fabm_models_imr OBJECT
    imr_model_library.F90
    imr_norwecom.F90
    imr_norwecom_num.F90
    NUMmodel/Fortran/read_input.f90
    NUMmodel/Fortran/globals.f90
    NUMmodel/Fortran/spectrum.f90
    NUMmodel/Fortran/generalists_simple.f90
    NUMmodel/Fortran/generalists.f90
    NUMmodel/Fortran/diatoms_simple.f90
    NUMmodel/Fortran/diatoms.f90
    NUMmodel/Fortran/copepods.f90
    NUMmodel/Fortran/POM.f90
    NUMmodel/Fortran/NUMmodel.f90
)

# Run the fypp preprocessor
# Code from https://fypp.readthedocs.io/en/stable/fypp.html#examples
set(fppFiles imr_norwecom_num.fpp)

foreach(infileName IN LISTS fppFiles)
    # Generate output file name
    string(REGEX REPLACE ".fpp\$" ".F90" outfileName "${infileName}")

    # Create the full path for the new file
    set(outfile "${CMAKE_CURRENT_BINARY_DIR}/${outfileName}")

    # Generate input file name
    set(infile "${CMAKE_CURRENT_SOURCE_DIR}/${infileName}")

    # Custom command to do the processing
    add_custom_command(
        OUTPUT "${outfile}"
        COMMAND fypp "${infile}" "${outfile}"
        MAIN_DEPENDENCY "${infile}"
        VERBATIM)

    # Finally add output file to a list
    set(outFiles ${outFiles} "${outfile}")
endforeach(infileName IN LISTS fppFiles)

add_dependencies(fabm_models_imr fabm_base)
