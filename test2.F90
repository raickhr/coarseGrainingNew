module ttest
    implicit none
    ! Define TestType
    type TestType
        real, allocatable :: a(:)

        contains    
            procedure :: add
            procedure :: subtract
            procedure :: multiply
            procedure :: allocate

    end type

    interface TestType
        MODULE PROCEDURE :: constructor
    end interface TestType


    contains

        function constructor() result(this)
            type(TestType) :: this
        end function constructor

        ! Procedure to add a scalar to the array
        subroutine add(this, scalar)
            class(TestType), intent(inout) :: this
            real, intent(in) :: scalar
            this%a = this%a + scalar
        end subroutine add

        ! Procedure to subtract a scalar from the array
        subroutine subtract(this, scalar)
            class(TestType), intent(inout) :: this
            real, intent(in) :: scalar
            this%a = this%a - scalar
        end subroutine subtract

        ! Procedure to multiply the array by a scalar
        subroutine multiply(this, scalar)
            class(TestType), intent(inout) :: this
            real, intent(in) :: scalar
            this%a = this%a * scalar
        end subroutine multiply

        ! Procedure to allocate memory for the array
        subroutine allocate(this, size)
            class(TestType), intent(inout) :: this
            integer, intent(in) :: size
            allocate(this%a(size))
        end subroutine allocate

end module ttest

program main
    use ttest
    implicit none

  
    ! Declare a variable of TestType
    type(TestType) :: myTest

    ! Allocate memory for the array
    call myTest%allocate(5)

  
    ! Initialize the array
    myTest%a = [1.0, 2.0, 3.0, 4.0, 5.0]
    

    ! Perform operations
    call myTest%add(2.0)
    call myTest%subtract(1.0)
    call myTest%multiply(3.0)

    ! Display the result
    print *, "Resultant array:"
    print *, myTest%a

end program main