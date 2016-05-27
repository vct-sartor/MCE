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

    private :: find

    ! Sample size
    integer, save :: ss
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

    write (*, "('Exploratory step...')")

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
    ss = count
    call iisort(space)

    rewind(input)
end subroutine exploratory_analysis

pure function find(x, v)
    ! Returns the index of the first occurrence of x in the vector
    ! v. If there is no ocurrence, it returns 0.

    implicit none

    integer :: find

    integer, intent(in) :: x, v(:)
    integer :: i

    do i = 1, size(v)
        if (v(i) .eq. x) then
            find = i
            return
        end if
    end do

    find = 0    
end function find

subroutine estimate
    ! Side-effects: Fills the saved variables referent to the
    ! transition matrix and probability vector.

    ! Specification: Assumues the saved variables referent to the
    ! sample space and sample size do be properly set with meaningful
    ! values.

    ! Specification: As per the module specification, expects the
    ! input cursor to be at the beginning of the file and at the
    ! return point rewinds it.

    implicit none

    integer :: i, j, n, vold, vcur, iold, icur

    write (*, "('Etimation step...')")

    n = size(space)

    allocate(probs(n))
    allocate(trans(n,n))

    do i = 1, n
        probs(i) = 0
        trans(i,:) = (/ (0, j=1,n) /)
    end do

    read (input, "(I12)") vcur
    icur = find(vcur, space)
    probs(icur) = probs(icur) + 1

    do i = 2, ss
        vold = vcur
        iold = icur

        read (input, "(I12)") vcur
        icur = find(vcur, space)

        trans(iold,icur) = trans(iold,icur) + 1
        probs(icur) = probs(icur) + 1
    end do

    probs = probs / sum(probs)

    do i = 1, n
        trans(i,:) = trans(i,:) / sum(trans(i,:))
    end do
end subroutine estimate

end module estimator
