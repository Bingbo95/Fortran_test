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
    !open(100, file = 'a.txt')                       !// ����һ�����ļ���д��ʵ������
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
    !open(100, file = 'a.txt', position = 'append')  !// ����position = 'append'��佫����ꡱ�����ļ�ĩβ
    !pause
    !backspace(100)                                  !// ʹ����ꡱ����һ��
    !!read(100,*) a                                   !// ��ȡ���һ������
    !!print*, a
    !!backspace(100)                                  !// �ٴκ���һ��
    !endfile(100)                                    !// ������һ������
    !pause
    !
    !
    !!**********����֮���spaceΪ2��������Ҫ����backspace.***********!
    !
    !backspace(100)                                  !// �ٴκ���һ��
    !backspace(100)                                  !// �ٴκ���һ��
    !endfile(100)                                    !// ������һ������
    !pause
    !
    !backspace(100)                                  !// �ٴκ���һ��
    !backspace(100)                                  !// �ٴκ���һ��
    !endfile(100)                                    !// ������һ������
    !!backspace(100)                                  !// �ٴκ���һ��
    !!!!!!! 9��8֮��������backspace
    !!read(100,*) b                                   !// ��ȡ���һ������
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