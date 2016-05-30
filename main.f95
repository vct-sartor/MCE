program mce
    ! Markov Chain Estimator
    ! Victhor S. Sartório

    ! Given an argument containing a filepath, it reads that file
    ! and estimates a transition matrix for it. Optionally, if given
    ! a second argument containing a new filepath, it saves the
    ! estimated transition matrix to it.

    use arguments, only: parse_arguments
    use file_handler, only: open_files
    use estimator, only: exploratory_analysis, estimate
    use simulator, only: simulate
    use reporter, only: report

    implicit none

    call parse_arguments

    call open_files

    call exploratory_analysis
    call estimate

    call simulate

    call report
end program mce
