!Gabriel Barreiro 100427354
!Licenciatura en fisica
!Tarea 4.1 INF 2120-04
!Universidad Autonoma de Santo Domingo 
!segundo semestre del 2023
program Tarea4o1GabrielBarreiro
  integer temp,i,j,k,n,A(100), B(100)
  print*, 'Cuantos elementos tiene el vector?'
  read*, n
  do I=1, n
  print*, 'Digite valores'
  read*, A(I)
  B(I) = A(I)
  enddo
  print*, 'vector original'
  do I=1, n
  print*, B(I)
  do J=1, n-1
  if (A(j).gt.A(J+1)) then
  temp=A(j)
  A(J)=A(J+1)
  A(J+1)=temp
  end if
  enddo
  enddo
  print*, 'vector ascendente'
  do I=1, n
  print*, A(I)
  enddo
  print*, 'vector descendente'
  do I=n, 1, -1
  print*, A(I)
  enddo
  read*, op
end program Tarea4o1GabrielBarreiro
      
