module simulator
    ! Markov Chain Estimator
    ! Simulator Module
    ! Victhor S. Sartório

    ! Is reponsible for using the estimations made by the estimator
    ! to simulate new observations.

    use arguments, only: steps
    use estimator, only: space, probs, trans

    integer, allocatable, save :: simulation(:)

contains

subroutine setprngseed
    ! Sets a very simple seed for the runtime prng
    
    implicit none

    integer :: ss, i, t
    integer, allocatable :: s(:)

    call random_seed(size = ss)
    allocate(s(ss))
    call system_clock(t)
    s = (/ (327*t*i, i=1,ss) /)
    call random_seed(put = s) 
end subroutine setprngseed


integer function isample(x)
    ! Sample the index weighted by their probabilities.

    implicit none

    double precision, intent(in) :: x(:)
    
    double precision, allocatable :: a(:)
    integer :: i
    real :: r

    allocate(a(size(x)))
    a(1) = x(1)
    do i = 1, size(a)
        a(i) = x(i) + a(i-1)
    end do

    call random_number(r)

    do i = 1, size(a)
        if (r .le. a(i)) then
            isample = i
            deallocate(a)
            return
        end if
    end do
    isample = size(a)
    deallocate(a)
end function isample


subroutine simulate
    ! Run the simulation.

    implicit none

    integer :: idx, n, i

    ! Initialization
    call setprngseed
    n = size(space)
    allocate(simulation(steps))

    ! Select the first value from the frequency matrix
    idx = isample(probs)
    simulation(1) = space(idx)

    ! Select the rest based on the transition matrix
    do i = 2, steps
        idx = isample(trans(idx,:))
        simulation(i) = space(idx)
    end do
end subroutine simulate

end module simulator