module arguments
    ! Markov Chain Estimator
    ! Arguments module
    ! Victhor S. Sartório

    ! Takes care of parsing the command line arguments and fetching
    ! the values into saved variables.

    ! Specification: The maximum length for the filepath in the
    ! arguments is of 126 characters. Should be plenty for pretty
    ! much any use case.

    ! Specification: The rightmost value always prevails when there
    ! are conflicts.

    ! Error codes:
    ! 100 - No input files
    ! 120 - Invalid flag

    implicit none

    character(len=52), private, parameter :: version_str = &
        "Markov Chain Estimator v0.2.2 - Victhor S. Sartório"
    character(len=69), private, parameter :: help_str =    &
        "Usage: mce [-v|-h|input_file] [-ooutput_file] [-fabsorbent|-funiform]"

    character(len=128), save :: input_path = "", output_path = "mce_report.dat"
    logical, save :: f_absorbent = .false., f_uniform = .false.

contains

subroutine parse_arguments
    ! Parses the arguments and assigns their values to the saved
    ! module variables.

    implicit none

    integer :: num_args, i
    character(len=128) :: arg

    num_args = command_argument_count()

    if (num_args .lt. 1) then
        write (*, "('mce: no input files')")
        error stop 100
    end if

    call get_command_argument(1, arg)

    select case (arg)
    case("--version", "-v")
        write (*, "(A)") version_str
        stop
    case("--help", "-h")
        write (*, "(A)") help_str
        stop
    case default
        input_path = arg
    end select

    do i = 2, num_args
        call get_command_argument(i, arg)

        select case (arg(1:2))
        case ("-o")
            output_path = arg(3:)

        case ("-f")
            if (trim(arg(3:)) .eq. "absorbent") then
                f_absorbent = .true.
                f_uniform = .false.
            else if (trim(arg(3:)) .eq. "uniform") then
                f_absorbent = .false.
                f_uniform = .true.
            else
                write (*, "('mce: expected [-funiform|-fabsorbent], got ', A)") trim(arg)
                error stop 120
            end if
            
        end select
    end do
end subroutine parse_arguments


end module arguments