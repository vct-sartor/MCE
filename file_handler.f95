module file_handler
    ! Markov Chain Estimator
    ! File Handler Module
    ! Victhor S. Sartório

    ! This module is responsible for handling the opening and, if
    ! applicable, reporting errors regarding them.

    ! Specification: The units used for input and output files are
    ! undefined since the NEW_UNIT feature from the Fortran 2008
    ! standard is being used for them.

    ! Error codes:
    ! 160 - Invalid input file
    ! 180 - Invalid outoput file

    use arguments, only: input_path, output_path

    implicit none

    integer, save :: input = 0, output = 0

contains

subroutine open_files
    ! Side-effect: Opens both files and saves their unit values on
    ! the "input" and "output" saved variables from the module.

    ! Specification: In case of invalid files, the iomsg may report
    ! the full file name plus the error message. Because of that, to
    ! be on the safe side, iomsg should have at least 100 characters
    ! plus the character parameters length.

    implicit none
    
    integer :: ioerr
    character(len=256) :: iomsg
    
    open(newunit = input, file = input_path, status = "old", &
         action = "read", iostat = ioerr, iomsg = iomsg)

    if (ioerr .ne. 0) then
        write (*, "(A, /, A)") "mce: invalid output file", trim(iomsg)
        error stop 160
    end if

    open(newunit = output, file = output_path, action = "write", &
         iostat = ioerr, iomsg = iomsg)

    if (ioerr .ne. 0) then
        write (*, "(A, /, A)") "mce: invalid output file", trim(iomsg)
        error stop 180
    end if
    
end subroutine open_files

end module file_handler
