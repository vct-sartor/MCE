module reporter
    ! Markov Chain Estimator
    ! Reporter Module
    ! Victhor S. Sartório

    ! This module implements the subroutine responsible for writing
    ! the results into the output files.

    use arguments, only: s_vprint
    use file_handler, only: output
    use estimator, only: ss, space, probs, trans
    use simulator, only: simulation

    implicit none

contains

subroutine report
    ! Side-effect: Writes the results of both the exploratory and
    ! inference steps to the file referrent to the output unit.

    ! Specification: Expects the exploratory and inference steps were
    ! completed successfully, and all the saved variables from the
    ! estimator unit hold meaningful values.

    implicit none

    integer :: n, i
    character(len=128) :: vfmt

    write (*, "('Generating report...')")

    n = size(space)
    
    ! Exploratory step

    write (output, "('c   Markov Chain Estimator - Inference Report', /,&
                    &'c   Victhor S. Sartório', /,&
                    &'c   Sample size', /, '    ', I0, /,&
                    &'c   State space')") ss

    write (vfmt, "('(''  ''', I0, '(''  '', I0))')") n

    write (output, vfmt) space

    ! Estimation step

    write (vfmt, "('(', I0, '(''    '', F12.10))')") n

    write (output, "('c   Probability vector')")
    write (output, vfmt) probs
    write (output, "('c   Transition matrix')")

    do i = 1, n
        write (output, vfmt) trans(i,:)
    end do

    ! Simulation step

    write (output, "('c   Simulation')")

    if (s_vprint) then
        do i = 1, size(simulation)
            write (output, "('    ', I0)") simulation(i)
        end do
    else
        write (vfmt, "('(', I0, '(''    '', I0))')") size(simulation)
        write (output, vfmt) simulation
    end if

end subroutine report

end module reporter
