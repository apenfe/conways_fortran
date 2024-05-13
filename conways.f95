PROGRAM vida
    USE conway
    USE conway_grafico
    USE estadisticas
          IMPLICIT NONE
          INTEGER:: generacion, celulas, respuesta, maxcel, apogeo, a(53,78), b(53,78), condicion, N, REGLA(8), WOLFRAM(53,78)
          REAL,ALLOCATABLE::interpolar(:,:),l(:,:),u(:,:), X(:),Y(:),Z(:)
           1 REWIND (0)
              REWIND (4)
                      maxcel = 0
                      generacion = 0
       OPEN (UNIT=0, FILE="probeta.dat")
       OPEN (UNIT=1, FILE="creacion.txt")
       OPEN (UNIT=2, FILE="funciones.txt")
       OPEN (UNIT=3, FILE="interpolacion.txt")
       OPEN (UNIT=4, FILE="WOLFRAM.dat")
        
    CALL presentacion (a=a, respuesta=respuesta, WOLFRAM=WOLFRAM)
       
    IF(respuesta==1)THEN
              CALL primeragen (a=a)
                generacion = 1
            write(*,*)
            write(*,*)"_hasta que generacion desea simular?_"
            read(*,*) condicion
         DO WHILE(generacion /= condicion)
              CALL procesoprincipal (a=a,b=b)
              CALL comparaciongeneraciones (a=a,b=b,celulas=celulas)
              CALL comentariogeneracion (generacion=generacion, celulas=celulas, maxcel=maxcel,apogeo=apogeo)
              CALL escriturageneracion (a=a)
                generacion = generacion + 1
         END DO
    
    ELSE IF(respuesta==2)THEN
    !-----------------------------------------------------------------------AUTOMATA DE WOLFRAM-------------------------------------------
       CALL primeragen (a=WOLFRAM)
       CALL procesoprincipalWOLFRAM(a=WOLFRAM,b=B, REGLA=REGLA)
       READ(*,*)
    GO TO 1
    ELSE IF(respuesta==3)THEN
         2 CALL opciones (respuesta=respuesta)
                              IF(respuesta==1)THEN
                                 CALL respuesta1 (a=a)
                                 GO TO 2
                              ELSE IF(respuesta==2)THEN
                                 CALL respuesta2 (a=a)
                                 GO TO 2
                              ELSE IF(respuesta==3)THEN
                                 CALL respuesta3 ()
                                 GO TO 2
                              ELSE IF(respuesta==4)THEN
                                 CALL respuesta4 ()
                                 GO TO 2
                              ELSE IF(respuesta==5)THEN
                                 GO TO 1
                              ELSE
                                 CALL respuesta6 ()
                                 GO TO 2       
                              END IF
    ELSE IF(RESPUESTA==4)THEN
        4 CALL opciones_calculo(respuesta)
                              IF(respuesta==1)THEN
                              REWIND(2)
                               CALL interpolacion(interpolar=INTERPOLAR,y=Y,n=N, x=X,Z=Z,U=U,L=L)
                               CALL factorizacion( interpolar=INTERPOLAR,n=N )
                               CALL separacion(interpolar=INTERPOLAR,l=L,u=U, n=N)
                               CALL resolucionsistema(u=U,l=L,n=N,z=Z,x=X,y=Y)
                               CALL funcioninterpolada(n=N,Y=Y)
                                 GO TO 4
                              ELSE IF(respuesta==2)THEN
                                CALL medidas_central(y=Y, n=N)
                                 GO TO 4
                              ELSE IF(respuesta==3)THEN
                                 GO TO 1
                              ELSE
                                 CALL respuesta6 ()
                                 GO TO 4       
                              END IF
    
    GO TO 1
    ELSE IF(RESPUESTA==5)THEN
    write(*,*)"Esta seguro que desea SALIR?"
    write(*,*)"  _si(0)_       _no(cualquier_tecla)_"
    read(*,*)respuesta
    if(respuesta==0)then
    GO TO 3
    end if
    go to 1
    ELSE
      CALL erroreleccion ()
      GO TO 1
    END IF
     CALL informacionproceso (maxcel=maxcel, apogeo=apogeo, generacion=generacion, respuesta=respuesta)
                      IF (respuesta==1)THEN
                      ELSE
                         GO TO 1
                      END IF
     3 CLOSE (0)
        CLOSE (1)
        CLOSE (2)
        CLOSE (3)
        CLOSE (4)
    END PROGRAM vida
    
    module conway
    contains
    !!-----------------------------------------------------------------------PROCESO PRINCIPAL-------------------------------------------------------------
    subroutine procesoprincipal(a,b)
    integer,intent(inout)::a(53,78), b(53,78)
    integer::i,j,m,z,total
    do i=1, 53
         do j=1, 78
                b(i,j)=0
         end do
    end do
    
                    do i=1, 53
                            do j=1, 78
    
                                      if(i==1)then                            !! BORDE SUPERIOR                          
                                                    if(j==1)then                 !! ESQUINA 1     
                                                                do z=i, i+1
                                                                    do m=j, j+1
                                                                           if(a(z,m)==1)then
                                                                                 total=total+1
                                                                           else
                                                                           end if
                                                                    end do
                                                                end do
                                                                            if(a(i,j)==0)then
                                                                                 if(total==3)then
                                                                                         b(i,j)=1
                                                                                 else
                                                                                 end if
                                                                            else if(a(i,j)==1)then
                                                                                     total=total-1
                                                                                  if((total==2).or.(total==3))then
                                                                                          b(i,j)=1
                                                                                  else
                                                                                          b(i,j)=0
                                                                                  end if
                                                                            end if
                                                    else if(j==78)then         !! ESQUINA 2                 
                                                                do z=i, i+1
                                                                    do m=j-1, j
                                                                           if(a(z,m)==1)then
                                                                                 total=total+1
                                                                           else
                                                                           end if
                                                                    end do
                                                                end do
                                                                            if(a(i,j)==0)then
                                                                                 if(total==3)then
                                                                                         b(i,j)=1
                                                                                 else
                                                                                 end if
                                                                            else if(a(i,j)==1)then
                                                                                       total=total-1
                                                                                  if((total==2).or.(total==3))then
                                                                                          b(i,j)=1
                                                                                  else
                                                                                          b(i,j)=0
                                                                                  end if
                                                                            end if
                                                    else                         !!borde superior general
                                                                do z=i, i+1
                                                                    do m= j-1, j+1
                                                                           if(a(z,m)==1)then
                                                                                 total=total+1
                                                                           else
                                                                           end if
                                                                    end do
                                                                end do
                                                                !!---------------------------infinito
                                                                do z=j-1 ,j+1
                                                                       if(a(53,z)==1)then
                                                                                 total=total+1
                                                                           end if
                                                                end do
                                                                !!--------------------------infinito
                                                                            if(a(i,j)==0)then
                                                                                 if(total==3)then
                                                                                        b(i,j)=1
                                                                                 else
                                                                                 end if
                                                                            else if(a(i,j)==1)then
                                                                                       total=total-1
                                                                                  if((total==2).or.(total==3))then
                                                                                           b(i,j)=1
                                                                                  else
                                                                                           b(i,j)=0
                                                                                  end if
                                                                            end if
                                                    end if
                                         else if(i==53)then                    !! BORDE INFERIOR                            
                                                    if(j==1)then                 !! ESQUINA 3                                      
                                                             do z=i-1, i
                                                                 do m= j, j+1
                                                                           if(a(z,m)==1)then
                                                                                  total=total+1
                                                                           else
                                                                           end if
                                                                 end do
                                                             end do
                                                                            if(a(i,j)==0)then
                                                                                  if(total==3)then
                                                                                          b(i,j)=1
                                                                                  else
                                                                                  end if
                                                                            else if(a(i,j)==1)then
                                                                                       total=total-1
                                                                                  if((total==2).or.(total==3))then
                                                                                              b(i,j)=1
                                                                                  else
                                                                                              b(i,j)=0
                                                                                  end if
                                                                            end if
                                          
                                          else if(j==78)then          !! ESQUINA 4                        
                                                      do z=i-1, i
                                                     do m=j-1, j
                                                          if(a(z,m)==1)then
                                                           total=total+1
                                                         else
                                                         end if
                                                     end do
                                                 end do
                                                  if(a(i,j)==0)then
                                                  if(total==3)then
                                                       b(i,j)=1
                                                   else
                                                   end if
                                            else if(a(i,j)==1)then
                                                    total=total-1
                                                   if((total==2).or.(total==3))then
                                                        b(i,j)=1
                                                   else
                                                        b(i,j)=0
                                                   end if
                                            end if
                                      
                                          else
                                                do z=i, i-1
                                                   do m= j-1, j+1
                                                          if(a(z,m)==1)then
                                                           total=total+1
                                                          else
                                                          end if
                                                   end do
                                                end do
                                                !!---------------------------infinito
                                                                do z=j-1 ,j+1
                                                                       if(a(1,z)==1)then
                                                                                 total=total+1
                                                                           end if
                                                                end do
                                                 !!--------------------------infinito
                                                 if(a(i,j)==0)then
                                                  if(total==3)then
                                                       b(i,j)=1
                                                   else
                                                   end if
                                            else if(a(i,j)==1)then
                                                    total=total-1
                                                   if((total==2).or.(total==3))then
                                                        b(i,j)=1
                                                   else
                                                        b(i,j)=0
                                                   end if
                                            end if
                                          end if
                                          
                               else if(j==1)then                    !! BORDE IZQUIERDO                  
                                          IF((I/=1).AND.(I/=53))THEN
                                                    do z=i-1, i+1
                                                       do m=j,  j+1
                                                             if(a(z,m)==1)then
                                                                total=total+1
                                                             else
                                                             end if
                                                       end do
                                                    end do
                                                    !!---------------------------infinito
                                                                do z=i-1 ,i+1
                                                                       if(a(z,78)==1)then
                                                                                 total=total+1
                                                                           end if
                                                                end do
                                                     !!--------------------------infinito
                                                     if(a(i,j)==0)then
                                                  if(total==3)then
                                                       b(i,j)=1
                                                   else
                                                   end if
                                            else if(a(i,j)==1)then
                                                    total=total-1
                                                   if((total==2).or.(total==3))then
                                                        b(i,j)=1
                                                   else
                                                        b(i,j)=0
                                                   end if
                                            end if
                                          ELSE
                                          END IF
                               else if(j==78)then                    !! BORDE DERECHO                      
                                          IF((I/=1).AND.(I/=53))THEN
                                                      do z=i-1, i+1
                                                         do m= j-1, j
                                                                 if(a(z,m)==1)then
                                                                    total=total+1
                                                                 else
                                                                 end if
                                                         end do
                                                      end do
                                                      !!---------------------------infinito
                                                                do z=i-1 ,i+1
                                                                       if(a(z,1)==1)then
                                                                                 total=total+1
                                                                           end if
                                                                end do
                                                     !!--------------------------infinito
                                                       if(a(i,j)==0)then
                                                  if(total==3)then
                                                       b(i,j)=1
                                                   else
                                                   end if
                                            else if(a(i,j)==1)then
                                                    total=total-1
                                                   if((total==2).or.(total==3))then
                                                        b(i,j)=1
                                                   else
                                                        b(i,j)=0
                                                   end if
                                            end if
                                          ELSE
                                          END IF
    !!GENERAL
                               else                                                             
                                       total=0
                                            do z=i-1, i+1
                                               do m= j-1, j+1
                                                      if(a(z,m)==1)then
                                                           total=total+1
                                                      else
                                                      end if
                                                end do
                                            end do
                                            if(a(i,j)==0)then
                                                  if(total==3)then
                                                       b(i,j)=1
                                                   else
                                                   end if
                                            else if(a(i,j)==1)then
                                                    total=total-1
                                                   if((total==2).or.(total==3))then
                                                        b(i,j)=1
                                                   else
                                                        b(i,j)=0
                                                   end if
                                            end if
                               end if
                 end do
        end do
    end subroutine procesoprincipal
    
    
    subroutine comparaciongeneraciones(a,b,celulas)
      integer,intent(inout)::a(53,78), b(53,78), celulas
      integer::z,m
        celulas=0
              do z=1, 53
                           do m=1, 78
                                      if(b(z,m)==0)then
                                             a(z,m)=0
                                      else
                                             a(z,m)=1
                                          celulas= celulas+1
                                      end if
                            end do
              end do  
    end subroutine comparaciongeneraciones
    
    
    subroutine comentariogeneracion(generacion, celulas, maxcel,apogeo)
    integer,intent(inout):: celulas, generacion, maxcel, apogeo
    WRITE(*,*) "_ GENERACION _: ", generacion
    WRITE(1,*) "_ GENERACION _: ", generacion
    WRITE(2,*)  generacion, celulas
    WRITE(*,*) "_ CELULAS _:", celulas
    WRITE(1,*) "_ CELULAS _:", celulas
       if(celulas>maxcel)then
         maxcel=celulas
         apogeo=generacion
       else
       end if
    end subroutine comentariogeneracion
    
    
    subroutine escriturageneracion(a)
    integer,intent(inout)::a(53,78)
    integer::i
    do i=1, 53
        write(*,"(78i1)") a(i,:)
        write(1,"(78i0)") a(i,:)
     end do
      write(*,*)
     write(1,*)
     READ(*,*)
    end subroutine escriturageneracion
    
    
    SUBROUTINE PRIMERAGEN(A)
    INTEGER,INTENT(INOUT)::A(53,78)
    INTEGER::I
    write(*,*)"_ZEPA INICIAL_"
                    do i=1 ,53
                          write(*,"(78i0)") a(i,:)
                    end do
       write(*,*)
    END SUBROUTINE PRIMERAGEN
    
    !!-----------------------------------------------------------------------FACTORIZACION LU-------------------------------------------------------
    SUBROUTINE interpolacion(interpolar,y,n, x, Z, U,L)
    real,allocatable,intent(inout):: interpolar(:,:), y(:), x(:), Z(:),U(:,:),L(:,:)
    integer,intent(inout)::n
    REAL::TIME
    integer:: i, j, nombre
    write(*,*)"calculando dimension \~"
    n=0
      do
         read(2,*,iostat=nombre)
            n=n+1
            if(nombre<0)exit
       end do
       n=n-1
       rewind(2)
       write(*,*)"alocatando arrays \~"
       allocate ( interpolar(1:n,1:n) )    
       allocate ( L(1:n,1:n) )  
       allocate ( U(1:n,1:n) )  
       allocate ( x(1:n) )
       allocate ( y(1:n) )
       allocate ( Z(1:n) )
      write(*,*)"arrays alocatados \~"
    do i= 1, n
      read(2,*) x(i), y(i)
    end do
    
    interpolar(1:n,1)=1
    do i=1, n
       do j=2, n
            interpolar(i,j)=x(i)**(j-1)
        end do
    end do
    call cpu_time(time)
    WRITE(*,*)TIME,"(SECONDS)"
    write(*,*)"dimension calculada \~"
    read(*,*)
    END SUBROUTINE interpolacion
    
    
    subroutine factorizacion( interpolar,n )
    real,intent(inout):: interpolar(:,:)
    integer,intent(inout)::n
    integer:: impar_1, impar_2, par_1, par_2, i, j, k
    real::time
    write(*,*)"factorizando \~"
    par_1=0
    par_2=1
    impar_1=1
    impar_2=2
    interpolar(1,1:n)=interpolar(1,1:n)      
    do k=2, n+n-1             
         if(mod(k,2)==0)then    
         write(*,*)"calculando\",N,"\ecuaciones\>", (k*100)/(n+n-1),"%"   
         call cpu_time(time)
         write(*,*)
         write(*,*) time,"(seconds)"
               if(k==2)then   
                     do i= k-par_1, n
                          interpolar(i,par_2) = interpolar(i,par_2)  / (interpolar(par_2,par_2))   
                     end do
               else
                     do i= k-par_1, n
                          interpolar(i,par_2) = (interpolar(i,par_2) &
                          - dot_product(interpolar(i,1:par_1),interpolar(1:par_1,par_2)) ) / (interpolar(par_2,par_2))             
                     end do
               end if
                     par_1=par_1+1    
                     par_2=par_2+1        
         else
         write(*,*)"calculando\",N,"\ecuaciones\>", (k*100)/(n+n-1),"%"    
         call cpu_time(time)
         write(*,*)
         write(*,*) time,"(seconds)"
                 do j= k-impar_1, n
                     interpolar(impar_2,j) = (interpolar(impar_2,j)&
                      - dot_product(interpolar(impar_2, 1:impar_1),interpolar(1:impar_1,j)) )  
                end do  
                 impar_1=par_1+1    
                    impar_2=impar_2+1                  
         end if       
    end do
    write(*,*)"factorizado \~"
    read(*,*)
    end subroutine factorizacion
    
    
    subroutine resolucionsistema(u,l,n,z,x,y)
     real,intent(inout)::u(:,:),l(:,:),z(:),x(:),y(:)
     integer,intent(inout)::n
     integer::i
     write(*,*)"calculando polinomio \~"
     z(1)=y(1)/l(1,1)
     do i=2, n
     z(i)= y(i) - dot_product(l(i,i+1:n) ,y(i+1:n))
         z(i)=z(i)/u(i,i)
     end do
     x(n)= z(n)/u(n,n)    
    DO i= n-1, 1, -1     
         x(i)= z(i) - dot_product(u(i,i+1:n) ,z(i+1:n))
         x(i)=z(i)/u(i,i)
    END DO
    write(*,*)"polinomio calculado \~"
    read(*,*)
    end subroutine
    
    
    subroutine separacion(interpolar,l,u, n)
    real,intent(inout)::interpolar(:,:), l(:,:), u(:,:)
    integer,intent(inout):: n
    integer::i, h
    write(*,*)"separando LU \~"
    u=0
    l=0
    h=1
    do i=1, n
    u(i,h:n)=interpolar(i,h:n)
      h=h+1
     end do
     h=1
     do i=2, n
     l(i,1:h)=interpolar(i,1:h)
    h=h+1
    end do
    do i=1,n
      l(i,i)=1
     end do
     write(*,*)"LU separada \~"
     read(*,*)
     end subroutine separacion
     
     
     subroutine funcioninterpolada(n,Y)
     real,intent(inout)::y(:)
     integer,intent(inout):: n
     integer::i
     write(*,*)"_ LA INTERPOLACION ES UN POLINOMIO DE GRADO_",N-1
     write(*,*)
     write(3,*)"_ LA INTERPOLACION ES UN POLINOMIO DE GRADO_",N-1
     write(3,*)
     do i=1, n
     write(*,*) y(i),"*x^",i-1
     write(*,*)
     write(3,*) y(i),"*x^",i-1
     write(3,*)
     end do
     READ(*,*)
     end subroutine funcioninterpolada
     
     !----------------------------------------------------------------------eliminacion gaussiana---------------------------------------------------------------------------------------------
     subroutine sustitucion_inversa(n,D,C,B)
    INTEGER,INTENT(INOUT)::n
    real,INTENT(INOUT):: D(:), C(:), B(:,:)
    integer::i
    D(n)= C(n)/B(n,n)    
    DO i= n-1, 1, -1     
         D(i)= C(i) - dot_product(B(i,i+1:n) ,D(i+1:n))
         D(i)=D(i)/B(i,i)
    END DO
    end subroutine sustitucion_inversa
    
    
    subroutine eliminacion_gaussiana(A,B,C,D,E,F,n)
    real, intent(inout)::A(:,:), B(:,:), C(:), D(:), E(:), F(:)
    integer, intent(inout):: n
    integer:: i, j, k, z, m, h, X
    real::eps, solucionuno, soluciondos,  l
    EPS= 1.0E-05
    DO k= 1, n-1
              DO i= 1, n
                  DO j= 1, n                                      
                            B(i,j) = A(i,j)
                            D(i) = C(i)
                               DO z= k+1, n
                                          IF((abs(a(k,k))==0).or.(abs(a(k,k))<eps))THEN
                                               do x= 1, n
                                                 e(x)=a(k,x)
                                                 solucionuno=c(k)
                                               end do
                                               do x= 1, n
                                                 f(x)=a(k+1,x)
                                                  soluciondos=c(k+1)
                                               end do
                                               do x= 1, n
                                                 A(K+1,x)=E(x)
                                                 c(k)=soluciondos
                                               end do
                                               do x= 1, n
                                                 A(K,x)=F(x)
                                                 c(k+1)=solucionuno
                                               end do
                                             END IF
                                           L = A(z,k) / A(k,k)       
                                     DO m= 1, n
                                               B(z,k)=0             
                                            DO h= k+1, n
                                                B(z,h) = A(z,h) - ( L * A(k,h) )
                                                D(z) = C(z) - ( L * C(k) )   
                                            END DO
                                     END DO
                              END DO
                  END DO
             END DO
             A = B
             C = D
    END DO
    end subroutine eliminacion_gaussiana
    
    
    subroutine procesoprincipalWOLFRAM(a,b, REGLA)
    integer,intent(inout)::a(53,78), b(53,78), REGLA(8)
    integer::i,j,z, NITER, W, CELULAS
    do i=1, 53
         do j=1, 78
                b(i,j)=0
         end do
    end do
    REWIND(2)
    WRITE(*,*)"-------------------------------------------------------------------------------"
       WRITE(*,*)"_ QUE NORMA DESEA UTILIZAR _?"
       WRITE(*,*)"_ DE UN VALOR ENTRE 100 Y 256, EN BINARIO _"
       WRITE(*,*)"_ ES DECIR DE 8 DIGITOS, CADA UNO DE ELLOS ENTRE 0 Y 1 _"
       READ(*,*)REGLA(:)
    WRITE(*,*)
    WRITE(*,*)"_ CUANTAS GENERACIONES DESEA GENERAR? _"
    READ(*,*)NITER
    WRITE(*,*)
    WRITE(*,*)"CARGANDO / REGLA />",REGLA(:)
    WRITE(*,*)"CARGANDO / NITER />", NITER
    WRITE(*,*)"REGLA / NITER / CARGADO/>"
    READ(*,*)
    DO W=1, NITER
             DO J= 1, 78
                  !!GENERAL
                  IF(A(W,J)==0)THEN
                         IF((A(W,J-1)==0).AND.(A(W,J+1)==0))THEN
                                 B(W+1,J)=REGLA(8)
                        ELSE IF((A(W,J-1)==0).AND.(A(W,J+1)==1))THEN
                                 B(W+1,J)=REGLA(7)
                          ELSEIF((A(W,J-1)==1).AND.(A(W,J+1)==0))THEN
                                B(W+1,J)=REGLA(4)
                         ELSE
                                B(W+1,J)=REGLA(3)
                         END IF
                  ELSE
                         IF((A(W,J-1)==0).AND.(A(W,J+1)==0))THEN
                                 B(W+1,J)=REGLA(6)
                        ELSE IF((A(W,J-1)==0).AND.(A(W,J+1)==1))THEN
                                 B(W+1,J)=REGLA(5)
                         ELSE IF((A(W,J-1)==1).AND.(A(W,J+1)==0))THEN
                                B(W+1,J)=REGLA(2)
                         ELSE
                                B(W+1,J)=REGLA(1)
                         END IF
                  END IF
             END DO
             CELULAS=0
             DO Z= 1, 53
                  DO I= 1, 78
                      IF(B(Z, I)==1)THEN
                         CELULAS=CELULAS+1
                         END IF
                  END DO
             END DO
             WRITE(*,*)"GENERACION:",W
             WRITE(*,*)"CELULAS:",CELULAS
             DO Z= 1, 53
             WRITE(*,"(78i1)") B(Z,:)
             END DO
             READ(*,*)
             A=B
    END DO
                   
    end subroutine procesoprincipalWOLFRAM
    
    end module conway
    
    
    
    
    
    module conway_grafico
    contains
    
    subroutine presentacion(a, respuesta, WOLFRAM)
    integer,intent(inout)::a(53,78), respuesta, WOLFRAM(53,78)
    integer:: i
           do i=1 ,53
                 read(0,*) a(i,:)
           end do
           do i=1 ,53
                 read(4,*) WOLFRAM(i,:)
           end do
       write(*,*)"##### _ LABORATORIO VIRTUAL _ #####"
       write(*,*)"[Version  15.03.2014]"
       write(*,*)"-------------------------------------------------------------------------------"
       write(*,*)"   QUE DESEA HACER?"
       WRITE(*,*)
       write(*,*)"(1) _ AUTOMATA BIDIMENSIONAL DE CONWAY _"
       write(*,*)
       write(*,*)"(2) _ AUTOMATA BIDIMENSIONAL DE WOLFRAM / FRACTALES _"
       write(*,*)
       write(*,*)"(3) _ HERRAMIENTAS / INFORMACION _"
       write(*,*)
       write(*,*)"(4) _ CALCULOS _"
       write(*,*)
       write(*,*)"(5) _ SALIR A WINDOWS/ LINUX/ ANDROID _"
       write(*,*)"-------------------------------------------------------------------------------"
       read(*,*) respuesta
    end subroutine presentacion
    
    
    subroutine informacionproceso(maxcel, apogeo, generacion, respuesta)
    integer,intent(inout)::respuesta
    integer,intent(inout):: maxcel, apogeo, generacion
    WRITE(*,*)
     write(*,*)"###_ INFORMACION DEL PROCESO: _###"
     write(*,*)"-------------------------------------------------------------------------------"
     write(*,*)"_ MAXIMO NUMERO DE CELULAS: _", maxcel
     write(*,*)"_ GENERACION: _", apogeo
     write(*,*)"_ NUMERO GENERACIONES TOTALES HASTA EXTINCION / ESTABILIZACION _",generacion-1
     write(*,*)"-------------------------------------------------------------------------------"
     read(*,*)
     WRITE(*,*)
     WRITE(*,*)"_ SALIR? _"
     WRITE(*,*)"______{1} SI_______{2} NO________"
     READ(*,*)RESPUESTA
    end subroutine informacionproceso
    
    
    subroutine respuesta6()
     write(*,*)"-------------------------------------------------------------------------------"
     write(*,*)"////// ERROR ////////"
     write(*,*)"-------------------------------------------------------------------------------"
    end subroutine respuesta6
    
    
    subroutine respuesta4()
    WRITE(*,*)"Juego de la vida, un universo bidimensional regido por unas leyes muy basicas:"
    WRITE(*,*)"si la casilla esta muerta y alrededor hay justamente 3 vivas,"
    WRITE(*,*)" entonces esta cobrara vida en la siguiente generacion"
    WRITE(*,*)
    read(*,*)
                 write(*,*)"000000000 000000000"
                 write(*,*)"000001100 000001100"
                 write(*,*)"000000000 000001000"
                 write(*,*)"000000100 000000100"
                 write(*,*)"000000000 000000000"
                 write(*,*)"000000000 000000000"
    read(*,*)
    WRITE(*,*)
    WRITE(*,*)"si la casilla esta viva y alrededor hay dos o tres vidas"
    WRITE(*,*)"se mantiene viva, sino muere"
    WRITE(*,*)
    read(*,*)
                 write(*,*)"000000000 000000000"
                 write(*,*)"000001100 000001100"
                 write(*,*)"000001000 000001000"
                 write(*,*)"000000100 000000100"
                 write(*,*)"000000000 000000000"
                 write(*,*)"000000000 000000000"
    WRITE(*,*)
                 write(*,*)"000000000 000000000"
                 write(*,*)"000001100 000001100"
                 write(*,*)"000001100 000000100"
                 write(*,*)"000000100 000000100"
                 write(*,*)"000000000 000000000"
                 write(*,*)"000000000 000000000"
                 read(*,*)
    WRITE(*,*)
    WRITE(*,*)"abrimos el archivo probeta.dat. ponemos alguna casilla diferente de 0"
    WRITE(*,*)"luego ejecutamos el programa, obtendremos resultado en creacion.dat"
    write(*,*)
    read(*,*)
    WRITE(*,*)"para mas informacion visite:   http://www.youtube.com/watch?v=GZ0qR69YnAw"
    write(*,*)"##    REALIZADO POR: ADRIAN PENALVER FERNANDEZ    ##"
    read(*,*)
    end subroutine respuesta4
    
    
    subroutine respuesta3()
    write(*,*)"_______________________________________________________________________________"
                 write(*,*)"_______________________________________________________________________________"
                 write(*,*)"_ PATRONES ESTABLES: _"
                 write(*,*)"-------------------------------------------------------------------------------"
                 write(*,*)"0000000000000000"
                 write(*,*)"0000000100000000"
                 write(*,*)"0011001010000100"
                 write(*,*)"0011001001001010"
                 write(*,*)"0000000110000100"
                 write(*,*)"0000000000000000"
                 write(*,*)
                 write(*,*)"Mantienen la posicion, hasta interactuar con otras formas."
                 WRITE(*,*)
                 read(*,*)
                 write(*,*)"_ NAVES ESPACIALES: _"
                 write(*,*)"-------------------------------------------------------------------------------"
                 write(*,*)"0000000000000000"
                 write(*,*)"0000000011000000"
                 write(*,*)"0000000101000000"
                 write(*,*)"0000000001000000"
                 write(*,*)"0000000000000000"
                 write(*,*)
                 write(*,*)"Se desplazan hasta que algo interfiere en sus trayectorias."
                 WRITE(*,*)
                 read(*,*)
                 write(*,*)"_ VARIANTES: _"
                 write(*,*)"-------------------------------------------------------------------------------"
                 write(*,*)"0000000000000000"
                 write(*,*)"0000000100000000"
                 write(*,*)"0000000100000000"
                 write(*,*)"0000000100000000"
                 write(*,*)"0000000000000000"
                 write(*,*)
                 write(*,*)"Realizan ciclos, volviendo siempre a la forma primigenia."
                 WRITE(*,*)
                 read(*,*)
                 write(*,*)"_ PENTONIO: _"
                 write(*,*)"-------------------------------------------------------------------------------"
                 write(*,*)"0000000000000000"
                 write(*,*)"0000001100000000"
                 write(*,*)"0000000110000000"
                 write(*,*)"0000000100000000"
                 write(*,*)"0000000000000000"
                 write(*,*)
                 write(*,*)"Patron especial para creacion de poblaciones estables."
                 read(*,*)
                 WRITE(*,*)
                 write(*,*)"_ GEOMETRICAS: _"
                 write(*,*)"-------------------------------------------------------------------------------"
                 write(*,*)"0000000000000000"
                 write(*,*)"0000000100000000"
                 write(*,*)"0000001010000000"
                 write(*,*)"0000010001000000"
                 write(*,*)"0000100000100000"
                 write(*,*)"0000111111100000"
                 write(*,*)"0000000000000000"
                 write(*,*)
                 write(*,*)"Forman patones muy curiosos."
                 read(*,*)
    end subroutine respuesta3
    
    
    subroutine respuesta2(a)
    integer,intent(inout)::a(53,78)
    integer:: respuesta, niter, i, j
    write(*,*)"_______________________________________________________________________________"
                 write(*,*)"_______________________________________________________________________________"
                 WRITE(*,*)"_ CUANTAS CASILLAS CAMBIARA? _"
                 READ(*,*) RESPUESTA
                 write(*,*)"-------------------------------------------------------------------------------"
                 NITER=0
                 DO WHILE(NITER<=RESPUESTA)
                 write(*,*)"_ INDIQUE SUBINDICES DE CASILLA :_ ",NITER+1
                 READ(*,*) I, J
                 write(*,*)"-------------------------------------------------------------------------------"
                 WRITE(*,*)"_ DE UN VALOR ENTRE 0 Y 1 _"
                 READ(*,*) RESPUESTA
                      A(I,J)=RESPUESTA
                 NITER=NITER+1
                 END DO
                 READ(*,*)
    end subroutine respuesta2
    
    
    subroutine respuesta1(a)
    integer,intent(inout)::a(53,78)
    integer::i,j, RESPUESTA
    write(*,*)"_______________________________________________________________________________"
                 write(*,*)"_______________________________________________________________________________"
                 write(*,*)"-------------------------------------------------------------------------------"
                 WRITE(*,*)"_CASILLAS UNA A UNA:_"  
                 write(*,*)"-------------------------------------------------------------------------------"
                 do i=1, 53
                     DO J=1, 78
                         write(*,*)"CASILLA:", I, J
                         write(*,*)
                        write(*,*)"VALOR =", a(i,J)
                        IF(A(I,J)==0)THEN
                        WRITE(*,*)"                           (MUERTA)"
                        ELSE
                        WRITE(*,*)"                              (VIVA)"
                        END IF
                        WRITE(*,*)
                        write(*,*)"SALIR? _1_"
                        READ(*,*)RESPUESTA
                        IF(RESPUESTA==1)THEN
                           GO TO 1
                        END IF
                        write(*,*)"-------------------------------------------------------------------------------"
                        READ(*,*)
                     END DO
                 end do
                 1 WRITE(*,*)"_ZEPA NATURAL:_"
                 do i=1, 53
                    write(*,"(78i1)") a(i,:)
                 end do  
                 READ(*,*)
    end subroutine respuesta1
    
    
    subroutine opciones(respuesta)
    integer,intent(inout)::respuesta
    write(*,*)"_______________________________________________________________________________"
     write(*,*)"_ HERRAMIENTAS / INFORMACION _"
     write(*,*)"-------------------------------------------------------------------------------"
     write(*,*)"(1) _ VER ZEPA: _"
     write(*,*)
     write(*,*)"(2) _ MODIFICAR PROBETA _"
     write(*,*)
     write(*,*)"(3) _ PATRONES ESPECIALES _"
     write(*,*)
     write(*,*)"(4) _ AYUDA _ "
     write(*,*)
     write(*,*)"(5) _ VOLVER _"
     write(*,*)"-------------------------------------------------------------------------------"
     read(*,*) respuesta
    end subroutine opciones
    
    
    subroutine erroreleccion()
    write(*,*)"-------------------------------------------------------------------------------"
     write(*,*)"////// ERROR ////////"
     write(*,*)"-------------------------------------------------------------------------------"
     write(*,*)"-------------------------------------------------------------------------------"
     write(*,*)"                   ########################################"
     write(*,*)"-------------------------------------------------------------------------------"
     write(*,*)
    end subroutine erroreleccion
    
    
    subroutine opciones_calculo(respuesta)
    integer,intent(inout)::respuesta
    write(*,*)"_______________________________________________________________________________"
     write(*,*)"_ CALCULOS _"
     write(*,*)"-------------------------------------------------------------------------------"
     write(*,*)"(1) _ INTERPOLAR POBLACION: _"
     write(*,*)
     write(*,*)"(2) _ MEDIDAS DE TENDENCIA CENTRAL [VARIABLE DISCRETA] _ "
     write(*,*)
     write(*,*)"(3) _ VOLVER _"
     write(*,*)"-------------------------------------------------------------------------------"
     read(*,*) respuesta
    end subroutine opciones_calculo
    
    
    end module conway_grafico
    
    
    
    
    module estadisticas
    implicit none
    contains
    
    subroutine medidas_central(y, n)
    real,intent(inout):: y(:)
    integer,intent(inout)::n
    real::x_i(n), frabsoluta( n), fracumulada(n), media
    real:: dimensiontabla, valor, total, moda, mediana, j, I
    !integer::i
    write(*,*)"cargando tabla \~"
    dimensiontabla=((maxval(y)-minval(y))+1)
    valor=minval(y)
    do i= 1, dimensiontabla
       x_i(i)=valor
       valor=valor+1
    end do
    
    do i=1, dimensiontabla
        total=0
        do j=1, n
             if(y(j)== x_i(i))then
               total=total+1
               end if
         end do
         frabsoluta(i)=total
    end do
    
    fracumulada(1)=frabsoluta(1)
     do i= 2, dimensiontabla
          fracumulada(i)= fracumulada(i-1)+frabsoluta(i)
     end do
     
     write(*,*)"tabla cargada \~"
     WRITE(*,*)"X_I         /       FR ABSOLUTA      /       FR ACUMULADA"
     DO I= 1, DIMENSIONTABLA
     WRITE(*,*) X_I(I), FRABSOLUTA(I), FRACUMULADA(I)
     END DO
     read(*,*)
     write(*,*)"calculando media aritmetica \~"
         total=0
      do i=1, dimensiontabla
          total= total+( x_i(i)*frabsoluta( i))
      end do
      media= total/ fracumulada(dimensiontabla)
      write(*,*)"media aritmetica calculada \~"
      read(*,*)
      write(*,*)"MEDIA=", x_i(i)
    !-----------------------------------------------------------------------MEDIANA----------------------------------------------------------------------------------------------------------------
    write(*,*)"calculando mediana \~"
      !mediana=(50/100)*fracumulada(dimensiontabla)
      !do i=1, dimensiontabla
       ! if(fracumulada(i)==mediana)then
        !    mediana=x_i(i)
        !else if(fracumulada(i)>mediana)then
         !     mediana=x_i(i)
          !end if
      !end do
      write(*,*)"mediana calculada \~"
      read(*,*)
      write(*,*)"MEDIANA=", x_i(i)
    !-----------------------------------------------------------------------MODA----------------------------------------------------------------------------------------------------------------------
    write(*,*)"calculando moda \~"
    write(*,*)"moda/s calculada/s \~"
    read(*,*)
       ! moda = maxval(frabsoluta)
       !  do i= 1, dimensiontabla
        !   if(frabsoluta(i)==moda)then
           write(*,*)"MODA=", x_i(i)
        !   end if
         !end do
    READ(*,*)
    end subroutine medidas_central
    
    end module estadisticas
    