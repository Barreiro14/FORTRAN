!Gabriel Barreiro 100427354
!Licenciatura en fisica
!Tarea 2 INF 2120-04
!Universidad Autonoma de Santo Domingo 
!segundo semestre del 2023
!vijirujunozoyuhusemirosinenoxuxaputudifo
program Tarea2GabrielBarreiro
    integer, dimension(40) :: numeros
    numeros = 0 !Pone el array en 0
    print *, "Introduzca entre 20 y 40 numeros naturales."
    do i = 1, 40 !Loop sobre el array completo
        read(*, *) val !Lee el valor introducido por el teclado
        if (val == -1) then !Si el valor es -1 sale del loop ya que no es un valor aceptado
            go to 200
        else
            numeros(i) = val !Se asignan los valores al iesimo elemento del array
        end if
    end do
    200 print *, "La secuencia ha sido introducida"
    if (count(numeros/=0) < 20) then !Revisa si almenos 20 valores han sido introducidos
        print *, "Se deben introducir al menos 20 valores"
    end if
    print *, "Suma de pares", "  Suma de impares"
    print *, SUM(numeros, MASK=MOD(numeros, 2) == 0), SUM(numeros, MASK=MOD(numeros, 2) == 1) !Usando algebra modular podemos saber si es par o no
end program Tarea2GabrielBarreiro