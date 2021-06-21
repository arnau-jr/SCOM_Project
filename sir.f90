module sir
      use network 
      use mtmod
      implicit none
      integer             :: N_inf,N_sus,E_act,N_rec
      integer,allocatable :: inf(:),act_link(:,:),P_act_link(:),states(:)
      real*8              :: lambda,delta

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
                              if(states(V_net(j)) == 1) then
                                    call rem_link(P_act_link(j))
                              else
                                    call add_link(choice,j)
                              end if
                        end do
                  end if
            end do
            N_sus = N_net-N_inf
            N_rec = 0
      end subroutine init_states

      subroutine rem_link(P_link)
            implicit none
            integer,intent(in) :: P_link
            integer :: i
            act_link(:,P_link) = act_link(:,E_act)
            do i=P_ini(act_link(1,P_link)),P_fin(act_link(1,P_link))
                  if(V_net(i) == act_link(2,P_link)) then
                        P_act_link(i) = P_link
                        exit
                  end if
            end do

            do i=P_ini(act_link(2,P_link)),P_fin(act_link(2,P_link))
                  if(V_net(i) == act_link(1,P_link)) then
                        P_act_link(i) = P_link
                        exit
                  end if
            end do

            act_link(:,E_act) = 0
            E_act = E_act - 1
      end subroutine rem_link
      
      ! subroutine rem_link(P_inf)
      !       implicit none
      !       integer :: P_inf
      !       integer :: j
      !       act_link(:,P_act_link(P_inf)) = act_link(:,E_act)

      !       !A: Implementacio extremadament cutre:
      !       !Mirem tots els punters dels veins del node infectat de l'ultim
      !       !link actiu.
      !       do j=P_ini(act_link(1,E_act)),P_fin(act_link(1,E_act))
      !             !Si el punter correspon a l'altre node del link
      !             if(V_net(j)==act_link(2,E_act)) then
      !                   !Substituim el punter corresponent pel punter nou
      !                   P_act_link(j) = P_act_link(P_inf)
      !                   exit !Sortim del loop, mes eficient.
      !             end if
      !       end do

      !       !El mateix pero ara mirem tots els punters del node no infectat
      !       !I canviem el punter del node infectat (per redundancia)
      !       !Tinc la sensacio de que aquest segon bucle es pot evitar, pero no estic segur
      !       do j=P_ini(act_link(2,E_act)),P_fin(act_link(2,E_act))
      !             if(V_net(j)==act_link(1,E_act)) then
      !                   P_act_link(j) = P_act_link(P_inf)
      !             end if
      !       end do
      !       P_act_link(P_inf) = 0
      !       !Netejem l'ultim link i reduim el compte de links
      !       act_link(:,E_act) = 0
      !       E_act = E_act - 1
      ! end subroutine rem_link

      subroutine add_link(new_inf,P_sus)
            implicit none
            integer,intent(in) :: new_inf,P_sus
            integer :: j

            E_act = E_act + 1
            act_link(:, E_act) = (/new_inf,V_net(P_sus)/)

            P_act_link(P_sus) = E_act
            do j=P_ini(V_net(P_sus)),P_fin(V_net(P_sus))
                  if(V_net(j)==new_inf) then
                        P_act_link(j) = E_act
                        exit !Podem sortir del loop un cop hem acabat.
                  end if
            end do
      end subroutine add_link

      subroutine infect_node(node)
            implicit none
            integer,intent(in) :: node

            integer :: i
            if (states(node) == 1) print *, "Trying to infect an infected node"

            do i = P_ini(node), P_fin(node)
                  if (states(V_net(i)) == 0) then
                        call add_link(node, i)
                  else if (states(V_net(i)) == 1) then
                        call rem_link(P_act_link(i))
                  end if
            end do
            states(node) = 1
            N_inf = N_inf + 1
            inf(N_inf) = node
            N_sus = N_sus - 1
      end subroutine infect_node

      subroutine recover_node(node)
            implicit none
            integer,intent(in) :: node

            integer :: i

            if (states(node) == 0) print *, "Trying to recover an susceptible node"
            if (states(node) == 2) print *, "Trying to recover an recovered node"

            do i = P_ini(node), P_fin(node)
                  if (states(V_net(i)) == 0) then
                        call rem_link(P_act_link(i))
                  end if
            end do
            states(node) = 2
            do i = 1, N_inf
                  if (inf(i) == node) then
                        inf(i) = inf(N_inf)
                        exit
                  end if
            end do
            inf(N_inf) = 0
            N_inf = N_inf - 1
            N_rec = N_rec + 1
      end subroutine recover_node
      
      subroutine sir_step(t)
            implicit none
            real*8,intent(inout) :: t
            real*8               :: prob_norm,prob_inf,prob_rec
            real*8               :: x
            integer              :: new_infected,new_recovered,node,unit

            ! unit = 2 !A: badly done

            prob_norm = N_inf*delta + E_act*lambda
            prob_inf = E_act*lambda/prob_norm
            prob_rec = N_inf*delta/prob_norm

            x = grnd()

            if(x<prob_inf) then
                  new_infected = choose_int(E_act)
                  ! write(unit,*)"Infecting link",act_link(:,new_infected)
                  node = act_link(2,new_infected)
                  call infect_node(node)
            else
                  new_recovered = choose_int(N_inf)
                  ! write(unit,*)"Recovering node",inf(new_recovered)
                  node = inf(new_recovered)
                  call recover_node(node)
            end if

            t = t + exp_number(prob_norm)
      end subroutine sir_step


      integer function choose_int(max) result (choice)
      ! Choose randomly an integer from 1 to max (inclusive)
            implicit none
            integer :: max
            real*8  :: x
            x = grnd()
            choice = ceiling(x*max)
      end function choose_int

      real*8 function exp_number(lmbd) result (x)
            implicit none
            real*8 :: lmbd
            x = -log(grnd())/lmbd
      end function exp_number

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