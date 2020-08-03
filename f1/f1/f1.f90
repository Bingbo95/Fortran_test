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
    !// 如果数据对象是静态的，在编译时就会给数据对象给定一定数据的内存，并且在执行期间直到程序退出才会释放内存；
    !// 如果数据对象时动态的，对象所需要的内存可以在程序运行的过程中被创建、修改和释放。
    !// 指针、可变数组和自动数组都是动态数据对象。
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
    !! pointer + target 相关联
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
    !!---二维数组赋初值，注意以“列”为主顺序进行排列
    !!data b/1,2,3,4,5,6,7,8,9,10,11,12/
    !data ((b(i,j),i=1,3),j=1,4)/1,2,3,4,5,6,7,8,9,10,11,12/
    !!-----------------------------------------
    !!---打印二维数组内容
    !do i=1,3
    !    write(*,*) b(i,:)
    !end do
    !!-----------------------------------------
    !!---引用二维数组第二行
    !a=>b(2,:)
    !write(*,*) 'a=>b(2,:) ',a
    !!-----------------------------------------
    !!---引用二维数组第三列
    !a=>b(:,3)
    !write(*,*) 'a=>b(:,3)  ',a
    !!------------------------------------
    !end program test_pointer_3

    !program test_pointer_4
    !implicit none
    !integer::i,j,k
    !real,pointer::p(:,:) !二维的指针
    !real,target::b(3,4)
    !!-----------------------------------------
    !!---二维数组赋初值
    !!data b/1,2,3,4,5,6,7,8,9,10,11,12/
    !data ((b(i,j),i=1,3),j=1,4)/1,2,3,4,5,6,7,8,9,10,11,12/
    !!-----------------------------------------
    !!---打印二维数组内容
    !write(*,*) 'b(3,4)'
    !do i=1,3
    !    write(*,*) b(i,:)
    !end do
    !!-----------------------------------------
    !!---引用二维数组中的二维片段
    !p=>b(1:2,2:3)
    !write(*,*) 'p=>b(1:2,2:3)'
    !do i=1,2
    !    write(*,*) p(i,:)
    !end do
    !!-----------------------------------------
    !p=0    !将b(1:2,2:3)中的元素置为0
    !!---打印二维数组内容
    !write(*,*) 'b(3,4)'
    !do i=1,3
    !    write(*,*) b(i,:)
    !end do
    !!------------------------------------
    !end program test_pointer_4

    !program test_pointer_5
    !implicit none
    !integer :: i
    !real,pointer :: p(:) !利用指针为数组动态分配内存
    !!-----------------------------------------
    !allocate(p(5))     !---为数组(指针)分配内存空间
    !!-----------------------------------------
    !!---数组赋值
    !do i=1,5
    !    p(i)=i*2
    !end do
    !!-----------------------------------------
    !write(*,*) 'p(5)=',p !p(5)=2,4,6,8,10
    !!-----------------------------------------
    !deallocate(p)   !---为数组(指针)释放内存空间
    !end program test_pointer_5


    ! 也可以这样、即不分配内存
    !program test_pointer_5
    !implicit none
    !integer :: i
    !real :: p(5) !利用指针为数组动态分配内存
    !!-----------------------------------------
    !!allocate(p(5))     !---为数组(指针)分配内存空间
    !!-----------------------------------------
    !!---数组赋值
    !do i=1,5
    !    p(i)=i*2
    !end do
    !!-----------------------------------------
    !write(*,*) 'p(5)=',p !p(5)=2,4,6,8,10
    !!-----------------------------------------
    !!deallocate(p)   !---为数组(指针)释放内存空间
    !end program test_pointer_5