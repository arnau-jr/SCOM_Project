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


            prob_norm = N_inf*delta + E_act*lambda
            prob_inf = E_act*lambda/prob_norm
            prob_rec = N_inf*delta/prob_norm

            x = grnd()

            if(x<prob_inf) then
                  new_infected = choose_int(E_act)
                  node = act_link(2,new_infected)
                  call infect_node(node)
            else
                  new_recovered = choose_int(N_inf)
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