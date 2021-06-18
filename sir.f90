module sir
      use network 
      use mtmod
      implicit none
      integer             :: N_inf,N_ina,E_act
      integer,allocatable :: inf(:),act_link(:,:),P_act_link(:),states(:)

      contains

      subroutine init_sir()
            implicit none

            allocate(inf(N_net),act_link(2,E_net),P_act_link(2*E_net),states(N_net))

      end subroutine init_sir

      ! subroutine get_state(unit,offset)
      !       implicit none
      !       integer :: counter,i,state

      !       inf = 0
      !       act_link = 0
      !       P_act_link = 0
            
      !       counter = 1
      !       do while(.true.)
      !             read(unit,*,end=20)i,state
                  
      !             if(state==1) then
      !                   inf(counter) = i
      !                   counter = counter + 1
      !             end if
      !       end do
      !       20 continue
      ! end subroutine get_state


      subroutine init_states(N)
            implicit none
            integer,intent(in) :: N
            integer            :: i,j,choice
            inf = 0
            act_link = 0
            E_act = 0
            P_act_link = 0
            states = 0

            N_inf = 0
            do while(N_inf<N)
                  choice = choose_int(N_net)
                  if(states(choice)==1) then
                        continue
                  else
                        N_inf = N_inf + 1
                        states(choice) = 1
                        inf(N_inf) = choice

                        do j=P_ini(choice),P_fin(choice)
                              print*, choice, V_net(j), states(V_net(j)),j
                              if(states(V_net(j)) == 1) then
                                    print*,"hola"
                                    call rem_link(j)
                              else
                                    call add_link(choice,j)
                              end if
                        end do
                  end if
            end do
            N_ina = N_net-N_inf
      end subroutine init_states

      subroutine rem_link(P_inf)
            implicit none
            integer :: P_inf
            integer :: j
            print *, act_link(:,P_act_link(P_inf))
            act_link(:,P_act_link(P_inf)) = act_link(:,E_act)

            !A: Implementacio extremadament cutre:
            !Mirem tots els punters dels veins del node infectat de l'ultim
            !link actiu.
            do j=P_ini(act_link(1,E_act)),P_fin(act_link(1,E_act))
                  !Si el punter correspon a l'altre node del link
                  if(V_net(j)==act_link(2,E_act)) then
                        !Substituim el punter corresponent pel punter nou
                        P_act_link(j) = P_act_link(P_inf)
                        exit !Sortim del loop, mes eficient.
                  end if
            end do

            !El mateix pero ara mirem tots els punters del node no infectat
            !I canviem el punter del node infectat (per redundancia)
            !Tinc la sensacio de que aquest segon bucle es pot evitar, pero no estic segur
            do j=P_ini(act_link(2,E_act)),P_fin(act_link(2,E_act))
                  if(V_net(j)==act_link(1,E_act)) then
                        P_act_link(j) = P_act_link(P_inf)
                  end if
            end do
            
            !Netejem l'ultim link i reduim el compte de links
            act_link(:,E_act) = 0
            E_act = E_act - 1
      end subroutine rem_link

      subroutine add_link(n_inf,P_inf)
            implicit none
            integer,intent(in) :: n_inf,P_inf
            integer :: j

            E_act = E_act + 1
            act_link(:, E_act) = (/n_inf,V_net(P_inf)/)

            P_act_link(P_inf) = E_act
            do j=P_ini(V_net(P_inf)),P_fin(V_net(P_inf))
                  if(V_net(j)==n_inf) then
                        P_act_link(j) = E_act
                        exit !Podem sortir del loop un cop hem acabat.
                  end if
            end do
      end subroutine add_link

      integer function choose_int(max) result (choice)
      ! Choose randomly an integer from 1 to max (inclusive)
            implicit none
            integer :: max
            real*8  :: x
            x = grnd()
            choice = ceiling(x*max)
      end function choose_int

      subroutine print_sir_info(unit)
            implicit none
            integer,intent(in) :: unit
            integer            :: i

            write(unit,*) "N_inf:", N_inf
            write(unit,*) "Act_links:", E_act
            
            write(unit,*) "Infected nodes"
            do i = 1, N_inf
                  write(unit,*) i,"|",inf(i)
            end do

            write(unit,*) "Active links"
            do i = 1, E_act
                  write(unit,*) i, "|", act_link(:,i)
            end do
            
            write(unit,*) "Current states"
            do i = 1, N_net
                  write(unit,*) i, "|", states(i)
            end do
      end subroutine print_sir_info
            
end module sir