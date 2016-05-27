program mce
    ! Markov Chain Estimator
    ! Victhor S. Sartório

    ! Given an argument containing a filepath, it reads that file
    ! and estimates a transition matrix for it. Optionally, if given
    ! a second argument containing a new filepath, it saves the
    ! estimated transition matrix to it.

    ! This file contains only argument parsing logic
    ! Actual program logic is found in auxiliary files

    ! Specification: The maximum length for the filepath in the
    ! arguments is of 128 characters. Should be plenty for pretty
    ! much any use case (I think/hope... It's a lot).

    ! Error codes:
    ! 100 - No input files

    use file_handler, only: open_files
    use estimator, only: exploratory_analysis, estimate
    use reporter, only: report

    implicit none

    character(len=64), parameter :: version_str = &
        "Markov Chain Estimator v0.2.0 - Victhor S. Sartório"
    character(len=64), parameter :: help_str = &
        "Usage: mce input_file [output_file]"

    integer :: num_args, i
    character(len=128) :: input_path, output_path, arg

    ! Argument handling

    num_args = command_argument_count()

    if (num_args .lt. 1) then
        write (*, "(A)") "mce: no input file"
        error stop 100
    end if

    call get_command_argument(1, input_path)

    select case (input_path)
    case("--version", "-v")
        write (*, "(A)") version_str
        go to 999
    case("--help", "-h")
        write (*, "(A)") help_str
        go to 999
    end select

    if (num_args .ge. 2) then
        call get_command_argument(2, output_path)
    else
        output_path = "mce_report.dat"
    end if

    if (num_args .gt. 2) then
        do i = 3, num_args
            call get_command_argument(i, arg)
            write (*, "(A, A)") "mce: warning: argument ignored: ", trim(arg)
        end do
    end if

    ! Program logic

    call open_files(input_path, output_path)

    call exploratory_analysis
    call estimate

    call report

999 continue
end program mce
