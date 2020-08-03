    !  f1.f90
    !
    !  FUNCTIONS:
    !  f1 - Entry point of console application.
    !

    !****************************************************************************
    !
    !  PROGRAM: f1
    !
    !  PURPOSE:  Entry point for the console application.
    !
    !****************************************************************************

    !program f1
    !
    !implicit none
    !
    !! Variables
    !
    !! Body of f1
    !print *, 'Hello World'
    !
    !end program f1

    !program main
    !
    !implicit none
    !integer :: a, b
    !
    !open(100, file = 'a.txt')                       !// 创建一个新文件并写入实验数据
    !write(100,*) 1
    !write(100,*) 2
    !write(100,*) 3
    !write(100,*) 4
    !write(100,*) 5
    !write(100,*) 6
    !write(100,*) 7
    !write(100,*) 8
    !write(100,*) 9
    !close(100)
    !
    !
    !pause
    !
    !
    !open(100, file = 'a.txt', position = 'append')  !// 利用position = 'append'语句将“光标”置于文件末尾
    !pause
    !backspace(100)                                  !// 使“光标”后退一行
    !!read(100,*) a                                   !// 读取最后一行数据
    !!print*, a
    !!backspace(100)                                  !// 再次后退一行
    !endfile(100)                                    !// 清除最后一行数据
    !pause
    !
    !
    !!**********数字之间的space为2，所以需要两个backspace.***********!
    !
    !backspace(100)                                  !// 再次后退一行
    !backspace(100)                                  !// 再次后退一行
    !endfile(100)                                    !// 清除最后一行数据
    !pause
    !
    !backspace(100)                                  !// 再次后退一行
    !backspace(100)                                  !// 再次后退一行
    !endfile(100)                                    !// 清除最后一行数据
    !!backspace(100)                                  !// 再次后退一行
    !!!!!!! 9和8之间有两个backspace
    !!read(100,*) b                                   !// 读取最后一行数据
    !!print*, b
    !close(100)
    !
    !end program main

    !// Pointer test of Fortran
    program test_pointer

    implicit none

    integer,pointer :: p1,p2
    integer :: s
    allocate(p1, p2)
    read(*,*) p1,p2
    s=p1+p2;
    write(*,*) 's = ',s
    deallocate(p1, p2)
    end program test_pointer