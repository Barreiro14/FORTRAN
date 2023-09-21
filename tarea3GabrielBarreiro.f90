!Gabriel Barreiro 100427354
!Licenciatura en fisica
!Tarea 3 INF 2120-04
!Universidad Autonoma de Santo Domingo
!segundo semestre del 2023
program Tarea3GabrielBarreiro
    implicit none
    character(100) :: name
    character(100) :: matricula
    character(100), dimension(8) :: asignaturas
    integer, dimension(8) :: creditos
    integer, dimension(8) :: notas
    character(100) :: asignatura
    integer :: nCreditos
    integer :: nNotas
    integer :: i
    integer :: j
    integer :: dProd
    real :: promedio

    creditos = 0
    notas = 0
    asignaturas = "NULL"

    print *, "Por favor introducir el nombre"
    read (*, '(A)') name
    print *, "Por favor introducir la matricula"
    read *, matricula
    print *, "Calculo de indice academico"
    
    100 do i = 1, 8
        print *, "Asignatura"
        read (*, '(A)') asignatura
        if (asignatura == "listo") then
            go to 200
        end if
        asignaturas(i) = asignatura
        print *, "Creditos"
        read *, nCreditos
        creditos(i) = nCreditos
        print *, "Nota"
        read *, nNotas
        notas(i) = nNotas
    end do

    200 if (count(creditos/=0) < 4 .and. count(creditos/=0) > 8) then !Revisa si almenos 4 valores han sido introducidos
        print *, "Se deben introducir al menos 4 y maximo 8 asignaturas, intente otra vez"
        go to 100
    end if

    dProd = DOT_PRODUCT(creditos, notas)
    promedio = dProd/SUM(creditos)
    print *, "Matricula: ",matricula,"Nombre: ",name
    print *, "---------------------------------------------------"
    print *, "NO. ", "Clave ", "CR. ", "Notas"
    do j = 1, count(creditos/=0)
        write(*, *) j, asignaturas(j), creditos(j), notas(j)
    end do
    print *, "---------------------------------------------------"
    print *, "Indice: ", promedio


    
  
end program Tarea3GabrielBarreiro
