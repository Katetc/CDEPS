module dglc_datamode_store_smbbuf_mod

    use shr_kind_mod     , only : r8=>shr_kind_r8, i8=>shr_kind_i8, cl=>shr_kind_cl, cs=>shr_kind_cs

    implicit none
    private
    
    ! Define the derived data type
    type, public :: smbbuf
        private
        real(r8), allocatable :: buffer(:,:)  ! 11 allocatable real arrays
        integer :: input_ptr
        integer :: chunk_size
    contains
        ! Type-bound procedures (accessor functions)
        procedure, public :: set_next_year_val
        procedure, public :: get_lag_year_val
        procedure, public :: initialize
        procedure, public :: print_buffer
        procedure, public :: deallocate_buffer
        procedure, public :: is_allocated
        procedure, public :: is_subarray_allocated
    end type smbbuf
    
contains

    ! Set the next year value in the circular array
    ! Use private pointer input_ptr that tracks where we
    ! are currently adding data
    ! Update the pointer AFTER setting the value
    subroutine set_next_year_val(this, data)
        class(smbbuf), intent(inout) :: this
        real(r8), intent(in) :: data(:)
        
        if (this%input_ptr >= 1 .and. this%input_ptr <= 11) then
            if (size(data) /= size(this%buffer,2)) then
                this%buffer(this%input_ptr,:) = data

                ! Update the local circular input pointer
                ! If less than 11, add one, if equal 11, set to 1
                if (this%input_ptr <=10) then
                    this%input_ptr = this%input_ptr+1
                else
                    this%input_ptr = 1
                endif
            endif
        else
            write(*,*) 'Error: Index out of bounds',this%input_ptr
        end if

    end subroutine set_next_year_val

    ! Get the 10 year lagging value from the array using a
    ! circular pointer system
    subroutine get_lag_year_val(this, subarray)
        class(smbbuf), intent(in) :: this
        real(r8), intent(out) :: subarray(:)
        integer :: lag_year = 0

        ! Set lag_year to the point in the circular buffer 10 
        ! elements prior to the current set point
        lag_year = this%input_ptr-10
        if(lag_year <= 0) lag_year = 11+lag_year

        if (lag_year >= 1 .and. lag_year <= 11) then
            subarray = this%buffer(lag_year,:)
        else
            write(*,*) 'Error: Index out of bounds',this%input_ptr
            return
        end if

    end subroutine get_lag_year_val

    ! Initialize buffer with a default value
    subroutine initialize(this, subarray_size, init_value)
        class(smbbuf), intent(inout) :: this
        integer, intent(in) :: subarray_size
        real(r8), intent(in), optional :: init_value
        real(r8) :: val
        
        if (present(init_value)) then
            val = init_value
        else
            val = 0.0_r8
        end if
        
        if (allocated(this%buffer)) then
            deallocate(this%buffer)
        end if
        
        allocate(this%buffer(11, subarray_size))
        this%buffer = val
        this%chunk_size = subarray_size

    end subroutine initialize
    
    ! Print the buffer contents for debugging
    subroutine print_buffer(this)
        class(smbbuf), intent(in) :: this
        integer :: i, j
        
        if (.not. allocated(this%buffer)) then
            write(*,*) 'Buffer not allocated'
            return
        end if
        
        write(*,*) 'SMBBuf contents:'
        write(*,'(A,I0,A,I0,A)') 'Buffer dimensions: ', size(this%buffer,1), ' x ', size(this%buffer,2)
        do i = 1, 11
            write(*,'(A,I2,A)', advance='no') 'Array ', i, ': ['
            do j = 1, size(this%buffer, 2)
                if (j < size(this%buffer, 2)) then
                    write(*,'(F6.2,A)', advance='no') this%buffer(i,j), ', '
                else
                    write(*,'(F6.2,A)') this%buffer(i,j), ']'
                end if
            end do
        end do
    end subroutine print_buffer

    ! Deallocate the buffer
    subroutine deallocate_buffer(this)
        class(smbbuf), intent(inout) :: this
        
        if (allocated(this%buffer)) then
            deallocate(this%buffer)
        end if
    end subroutine deallocate_buffer
    
    ! Check if buffer is allocated
    function is_allocated(this) result(status)
        class(smbbuf), intent(in) :: this
        logical :: status
        
        status = allocated(this%buffer)
    end function is_allocated
    
    ! Check if a specific subarray is allocated (always true if buffer is allocated in 2D implementation)
    function is_subarray_allocated(this, array_index) result(status)
        class(smbbuf), intent(in) :: this
        integer, intent(in) :: array_index
        logical :: status
        
        if (array_index < 1 .or. array_index > 11) then
            status = .false.
        else
            status = allocated(this%buffer)
        end if
    end function is_subarray_allocated

end module dglc_datamode_store_smbbuf_mod