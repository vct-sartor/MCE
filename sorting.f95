module sorting
    ! Markov Chain Estimator
    ! Sorting Module
    ! Victhor S. Sartório

    ! Implements a basic insertion sort.

    implicit none

contains

subroutine iisort(a)
    implicit none
    
    integer, allocatable, intent(inout) :: a(:)
    integer :: i, j, p

    if (.not. allocated(a)) error stop 999

    do i = 2, size(a)
        p = a(i)
        j = i - 1
        do while (j .ge. 1 .and. a(j) .gt. p)
            a(j+1) = a(j)
            j = j - 1
        end do
        a(j+1) = p
    end do
end subroutine iisort
    
end module
