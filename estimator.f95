module estimator
    ! Markov Chain Estimator
    ! Estimator Module
    ! Victhor S. Sartório

    ! This module is responsible for going through the input file
    ! and providing all desired results for it as module variables.

    ! Specification: All subroutines expect the input cursor to be at
    ! the beginning of the file and at the return point they should
    ! rewind it.

    ! Error codes:
    ! 220 - Failure while parsing file
    ! 240 - Expected to use unallocated variable
    ! 320 - Invalid sample size

    use iso_fortran_env, only: iostat_end
    use sorting, only: iisort
    use file_handler, only: input

    implicit none

    ! Sample size
    integer, save :: n
    ! State space
    integer, allocatable, save :: space(:)
    ! Probability vector and Transition matrix
    double precision, allocatable, save :: probs(:), trans(:,:)

contains

subroutine exploratory_analysis
    ! Side-effects: Fills the saved variables referent to the sample
    ! size and the state space.

    ! Specification: As per the module specification, expects the
    ! input cursor to be at the beginning of the file and at the
    ! return point rewinds it.

    implicit none

    integer, parameter :: bufsize = 16

    integer :: count, value, unique_count, unique_size
    integer, allocatable :: uniques(:), swap(:)

    integer :: i
    logical :: found_flag

    integer :: ioerr
    character(len=128) :: iomsg

    allocate(uniques(bufsize))
    unique_count = 0
    unique_size = bufsize

    count = 0

    do
        read (input, "(I12)", iostat = ioerr, iomsg = iomsg) value

        if (ioerr .eq. iostat_end) then
            exit
        else if (ioerr .ne. 0) then
            write (*, "(A, /, A)") "mce: Invalid entry in file", trim(iomsg)
            error stop 220
        end if

        count = count + 1

        found_flag = .false.
        do i = 1, unique_count
            if (value .eq. uniques(i)) then
                found_flag = .true.
                exit
            end if
        end do

        if (.not. found_flag) then
            if (unique_count .eq. unique_size) then
                allocate(swap(unique_count))
                swap = uniques
                deallocate(uniques)
                allocate(uniques(unique_size + bufsize))
                uniques(1:unique_size) = swap
                deallocate(swap)
                unique_size = unique_size + bufsize
            end if

            unique_count = unique_count + 1
            uniques(unique_count) = value
        end if
    end do

    if (count .le. 2) then
        write (*, "(A, I0, A)") "mce: Invalid sample size (", count, ")"
        error stop 320
    end if

    allocate(space(unique_count))
    space = uniques(1:unique_count)
    deallocate(uniques)
    n = count
    call iisort(space)
end subroutine exploratory_analysis

end module estimator
