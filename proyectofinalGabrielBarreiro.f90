        program project_barry
        implicit none
        
        character*20 dataname
        integer ans,stat,total

1       call system ('cls')

        write(*,10)'Programa Universitario CASP 2.1'
        print*,'[1] Entrar como estudiante.'
        print*,'[2] Entrar como profesor.'
        print*,'[3] Administrar cuentas.'
        print*,'[4] Graduacion.'
        print*
        print*
        print*,'[5] Salir.'
        read *,ans
        
        if (ans.eq.1) call student()
        if (ans.eq.2) call professor()
        if (ans.eq.3) call admin()
        if (ans.eq.4) call congrat()
        if (ans.eq.5) go to 2
        
10      format(2X,A31,10X,//)

        go to 1
2       end program project_barry
C     ------------------------------------------------------------------
C       Subprograma 1
C     ------------------------------------------------------------------
        subroutine student()
        implicit none
        
        character*20 dataname,name01,name02,surn01,surn02
        character*20 NN(100),NS(100),AN(100),AS(100)
        integer i,n,M,ans01,ans02,regis,stat,ISS(100)
        
1       call system ('cls')
        
        write(*,10)'Modulo estudiantil'
        print*,'[1] Iniciar seccion.'
        print*,'[2] Inscribirse.'
        write(*,11)
        print*,'[3] Salir.'
        read *,ans01

        if (ans01.eq.2) go to 2
        if (ans01.eq.1) call inscribir()
        if (ans01.eq.3) go to 4
        
7       call system ('cls')
        print*,'//Ya existe una cuenta con estos nombres y apellidos//'
        
2       call system ('cls')

        print*,'Coloque su nombre:'
        read*,name01
        print*,'Coloque su segundo nombre (sino: NA):'
        read*,name02
        print*,'Coloque su primer apellido:'
        read*,surn01
        print*,'Coloque su segundo apellido (sino: NA):'
        read*,surn02
        
        call system ('cls')
        
        print*,'Seguro que quiere registrarse como:'
        print*;print*,'Primer nombre:    ',name01
        if (name02.ne.'NA') print*,      'Segundo nombre:   ',name02
        print*,'Primer apellido:  ',surn01
        if (surn02.ne.'NA') print*,'Segundo apellido: ',surn02
        print*;print*,'[1] Borrar y reiniciar [2] Continuar'
5       read*,ans02

        open(1,file='cuentas.dat',status='unknown')
        i=1;do while (ISS(i).eq.0);i=i+1
        read(1,12)ISS(i),NN(i),NS(i),AN(i),AS(i)
        end do
        
        n      = i + 1
        name01 = NN(n)
        name02 = NS(n)
        surn01 = AN(n)
        surn02 = AS(n)
        M      = ISS(i)
        regis  = ISS(n)

        do i=1,M
        if (name01.eq.NN(i).and.name02.eq.NS(i).and.surn01.eq.AN(i)
     &  .and.surn02.eq.AS(i)) go to 7
        end do

        if (ans02.eq.1) go to 2
        if (ans02.eq.2) go to 6
        if (ans02.ne.1.and.ans02.ne.2) go to 5
        
6       rewind 1
        do i=1,M
        write(1,12)ISS(i),NN(i),NS(i),AN(i),AS(i)
        end do
        write(1,12)regis-regis,name01,name02,surn01,surn02
        close(1)
        print*,'Tu matricula es: ',regis
        read*
        go to 4
        
10      format(2X,A18,10X,//)
11      format(//)
12      format(I3,A20,2X,A20,2X,A20,2X,A20)

        go to 1
4       end subroutine student
C     ------------------------------------------------------------------
C       Subprograma 2
C     ------------------------------------------------------------------
        subroutine inscribir()
        implicit none
        
        character*20 NN(100),NS(100),AN(100),AS(100)
        integer      i,mat,M,ans01,ISS(100)
        
        open(1,file='cuentas.dat',status='unknown')
        i=1;do while (ISS(i).eq.0);i=i+1
        read(1,12) ISS(i),NN(i),NS(i),AN(i),AS(i)
        end do
        close(1)
        ISS(i)=M
        
1       call system ('cls')

        print*,'Escribe tu matricula para accesar'
        read *,mat
        
        if (mat.gt.M.or.mat.eq.0) go to 2
        
        print*,NN(mat),' ',NS(mat),' ',AN(mat),' ',AS(mat)
        print*,'[1]Seleccionar asignatura'
        print*,'[2]Revisar notas'
        print*;print*
        print*,'[3] Salir'
        read*,ans01
        
        if (ans01.eq.1) go to 3
        if (ans01.eq.2) call notas(mat)
        if (ans01.eq.3) go to 9
        
2       call system ('cls')
        print*,'No existe esa matricula'
        read *
        go to 1

12      format(I3,A20,2X,A20,2X,A20,2X,A20)
3       read*
9       end subroutine inscribir
C     ------------------------------------------------------------------
C       Subprograma 2
C     ------------------------------------------------------------------
        subroutine notas(mat)
        implicit none
        
        character*20 NN(100),NS(100),AN(100),AS(100)
        integer      mat,M,ans01,ISS(100),i
        
        open(1,file='cuentas.dat',status='unknown')
        i=1;do while (ISS(i).eq.0);i=i+1
        read(1,12)ISS(i),NN(i),NS(i),AN(i),AS(i)
        end do
        close(1)
        ISS(i)=M
        
        read*
        
12      format(I3,A20,2X,A20,2X,A20,2X,A20)
        
        end subroutine notas
C     ------------------------------------------------------------------
C       Subprograma 2
C     ------------------------------------------------------------------
        subroutine professor()
        implicit none

        end subroutine professor
C     ------------------------------------------------------------------
C       Subprograma 3
C     ------------------------------------------------------------------
        subroutine admin()
        implicit none

        end subroutine admin
C     ------------------------------------------------------------------
C       Subprograma 4
C     ------------------------------------------------------------------
        subroutine congrat()
        implicit none

        end subroutine congrat

