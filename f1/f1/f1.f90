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
    !// ������ݶ����Ǿ�̬�ģ��ڱ���ʱ�ͻ�����ݶ������һ�����ݵ��ڴ棬������ִ���ڼ�ֱ�������˳��Ż��ͷ��ڴ棻
    !// ������ݶ���ʱ��̬�ģ���������Ҫ���ڴ�����ڳ������еĹ����б��������޸ĺ��ͷš�
    !// ָ�롢�ɱ�������Զ����鶼�Ƕ�̬���ݶ���
    !program test_pointer_1
    !
    !implicit none
    !
    !integer,pointer :: p1,p2
    !integer :: s
    !allocate(p1, p2)
    !read(*,*) p1,p2
    !s=p1+p2;
    !write(*,*) 's = ',s
    !deallocate(p1, p2)
    !end program test_pointer_1



    !program test_pointer_2
    !
    !implicit none
    !! pointer + target �����
    !integer, pointer :: a(:)
    !integer, target  :: b(5)=(/1,2,3,4,5/)
    !a=>b
    !write(*,*) a
    !a=>b(2:3)
    !write(*,*) a
    !a=>b(5:1:-1)
    !write(*,*) a
    !write(*,*) b(3:4)
    !end program test_pointer_2


    !program test_pointer_3
    !implicit none
    !Integer::i,j,k
    !real,pointer::a(:)
    !real,target::b(3,4)
    !!-----------------------------------------
    !!---��ά���鸳��ֵ��ע���ԡ��С�Ϊ��˳���������
    !!data b/1,2,3,4,5,6,7,8,9,10,11,12/
    !data ((b(i,j),i=1,3),j=1,4)/1,2,3,4,5,6,7,8,9,10,11,12/
    !!-----------------------------------------
    !!---��ӡ��ά��������
    !do i=1,3
    !    write(*,*) b(i,:)
    !end do
    !!-----------------------------------------
    !!---���ö�ά����ڶ���
    !a=>b(2,:)
    !write(*,*) 'a=>b(2,:) ',a
    !!-----------------------------------------
    !!---���ö�ά���������
    !a=>b(:,3)
    !write(*,*) 'a=>b(:,3)  ',a
    !!------------------------------------
    !end program test_pointer_3

    !program test_pointer_4
    !implicit none
    !integer::i,j,k
    !real,pointer::p(:,:) !��ά��ָ��
    !real,target::b(3,4)
    !!-----------------------------------------
    !!---��ά���鸳��ֵ
    !!data b/1,2,3,4,5,6,7,8,9,10,11,12/
    !data ((b(i,j),i=1,3),j=1,4)/1,2,3,4,5,6,7,8,9,10,11,12/
    !!-----------------------------------------
    !!---��ӡ��ά��������
    !write(*,*) 'b(3,4)'
    !do i=1,3
    !    write(*,*) b(i,:)
    !end do
    !!-----------------------------------------
    !!---���ö�ά�����еĶ�άƬ��
    !p=>b(1:2,2:3)
    !write(*,*) 'p=>b(1:2,2:3)'
    !do i=1,2
    !    write(*,*) p(i,:)
    !end do
    !!-----------------------------------------
    !p=0    !��b(1:2,2:3)�е�Ԫ����Ϊ0
    !!---��ӡ��ά��������
    !write(*,*) 'b(3,4)'
    !do i=1,3
    !    write(*,*) b(i,:)
    !end do
    !!------------------------------------
    !end program test_pointer_4

    !program test_pointer_5
    !implicit none
    !integer :: i
    !real,pointer :: p(:) !����ָ��Ϊ���鶯̬�����ڴ�
    !!-----------------------------------------
    !allocate(p(5))     !---Ϊ����(ָ��)�����ڴ�ռ�
    !!-----------------------------------------
    !!---���鸳ֵ
    !do i=1,5
    !    p(i)=i*2
    !end do
    !!-----------------------------------------
    !write(*,*) 'p(5)=',p !p(5)=2,4,6,8,10
    !!-----------------------------------------
    !deallocate(p)   !---Ϊ����(ָ��)�ͷ��ڴ�ռ�
    !end program test_pointer_5


    ! Ҳ�������������������ڴ�
    !program test_pointer_5
    !implicit none
    !integer :: i
    !real :: p(5) !����ָ��Ϊ���鶯̬�����ڴ�
    !!-----------------------------------------
    !!allocate(p(5))     !---Ϊ����(ָ��)�����ڴ�ռ�
    !!-----------------------------------------
    !!---���鸳ֵ
    !do i=1,5
    !    p(i)=i*2
    !end do
    !!-----------------------------------------
    !write(*,*) 'p(5)=',p !p(5)=2,4,6,8,10
    !!-----------------------------------------
    !!deallocate(p)   !---Ϊ����(ָ��)�ͷ��ڴ�ռ�
    !end program test_pointer_5

    !program calling_func
    !implicit none
    !real :: x, y, z, disc, w
    !x = 1.0
    !y = 5.0
    !z = 2.0
    !call intent_example(x, y, z, disc)
    !Print *, "The value of the discriminant is"
    !Print *, disc
    !w = 3.0
    !
    !call intent_example(w, y, z, disc)
    !Print *, "The value of the discriminant is"
    !Print *, disc
    !end program calling_func
    !
    !subroutine intent_example (a, b, c, d)
    !implicit none
    !! dummy arguments
    !real, intent (inout) :: a
    !real, intent (in) :: b
    !real, intent (in) :: c
    !real, intent (out) :: d
    !logical :: fc = .TRUE.
    !save fc
    !if (fc) then
    !    a=5
    !    d = b * b - 4.0 * a * c
    !    Print *, "The value of the discriminant is 1"
    !else
    !    d=0
    !    Print *, "The value of the discriminant is 2"
    !end if
    !if (fc) then
    !    fc = .FALSE.
    !    Print *, "The value of the discriminant is 3"
    !end if
    !if (fc == .FALSE.) then
    !    print *, "ERROR!"
    !end if
    !end subroutine intent_example

    !program func
    !integer  :: b(5)=(/1,2,3,4,5/)
    !print *, b(5)   ! �������ܴ�0��ʼ����СΪ1.
    !end program func
    
    