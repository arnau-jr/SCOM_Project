module network
      implicit none
      integer             :: N_net,E_net,D_max
      integer,allocatable :: V_net(:),P_ini(:),P_fin(:),D_net(:),D_count(:)
      real*8              :: avgD,avgD2
      ! Explanation of varibles:
      !     - N_net, E_net, D_max: number of nodes, edges and maximum degree of the network
      !     - V_net, P_ini, P_fin: bond graph and pointers to access it efficiently.
      !     - D_net, D_count: degrees of each node and total amount of nodes for each degree
      !     - avgD, avgD2: average degree and square degree (<k> and <k^2>)
      contains

      subroutine init_net(unit,offset)
            implicit none
            integer :: unit, offset

            call get_ne(unit,offset)
            allocate(V_net(2*E_net),P_ini(N_net),P_fin(N_net),D_net(N_net))
            V_net = 0
            P_ini = 0
            P_fin = 0
            D_net = 0

            call get_degrees(unit,offset)
            allocate(D_count(0:D_max))

            call get_pointers(unit,offset)

            call get_average_degree()
      end subroutine init_net

      subroutine get_ne(unit,offset)
            implicit none
            integer :: unit,offset
            integer :: E
            integer :: i,j,i_max,j_max
            rewind(unit)
            do i=1,offset
                  read(unit,*)
            end do

            E = 0
            i_max = 0
            j_max = 0

            do while(.true.)
                  read(unit,*,end=20)i,j
                  
                  if(i>i_max) i_max = i
                  if(j>j_max) j_max = j
                  E = E + 1
            end do
            20 continue

            E_net = E
            if(i_max >= j_max) then
                  N_net = i_max
            else
                  N_net = j_max
            end if
      end subroutine get_ne

      subroutine get_degrees(unit,offset)
            implicit none
            integer :: unit,offset
            integer :: i,j
            rewind(unit)
            do i=1,offset
                  read(unit,*)
            end do

            D_net = 0
            do while(.true.)
                  read(unit,*,end=20)i,j

                  D_net(i) = D_net(i) + 1
                  D_net(j) = D_net(j) + 1
            end do
            20 continue
            D_max = maxval(D_net)
      end subroutine get_degrees


      subroutine get_pointers(unit,offset)
            implicit none
            integer :: unit,offset
            integer :: i,j
            rewind(unit)
            do i=1,offset
                  read(unit,*)
            end do

            !Prepare pointers
            P_ini(1) = 1
            P_fin(1) = 0
            do i=2,N_net
                  P_ini(i) = P_ini(i-1) + D_net(i-1)
                  P_fin(i) = P_ini(i) - 1
            end do

            do while(.true.)
                  read(unit,*,end=20)i,j

                  P_fin(i) = P_fin(i) + 1
                  P_fin(j) = P_fin(j) + 1

                  V_net(P_fin(i)) = j
                  V_net(P_fin(j)) = i
            end do
            20 continue
      end subroutine get_pointers

      subroutine get_average_degree()
            implicit none
            integer :: i
            avgD = 0.d0
            avgD2 = 0.d0
            D_count = 0
            do i=1,N_net
                  avgD = avgD + D_net(i)
                  avgD2 = avgD2 + D_net(i)**2

                  D_count(D_net(i)) = D_count(D_net(i)) + 1 
            end do
            avgD = avgD/N_net
            avgD2 = avgD2/N_net
      end subroutine get_average_degree

      subroutine print_info(unit)
            implicit none
            integer :: i,unit

            write(unit,*)"Nodes:",N_net
            write(unit,*)"Edges:",E_net
            write(unit,*)"Average degree:",avgD
            write(unit,*)"Average degree squared:",avgD2
            write(unit,*)""
            write(unit,*)"Degree | Count"
            do i=0,D_max
                  write(unit,*)i,"|",D_count(i)
            end do
            write(unit,*)
            write(unit,*)"Node        | Neighbours"
            do i=1,N_net
                  write(unit,*)i,"|",V_net(P_ini(i):P_fin(i))
            end do
      end subroutine print_info

      subroutine deallocate_network()
            implicit none

            if (allocated(V_net)) deallocate(V_net)
            if (allocated(P_ini)) deallocate(P_ini)
            if (allocated(P_fin)) deallocate(P_fin)
            if (allocated(D_net)) deallocate(D_net)
      end subroutine deallocate_network

!------------------------------- Correlations ----------------------------------

      subroutine get_degree_histogram(histo,histo_acu)
            implicit none
            real*8,intent(out) :: histo(D_max),histo_acu(D_max)
            integer            :: i
            
            do i=1,N_net
                  histo(D_net(i)) = histo(D_net(i)) + 1
                  histo_acu(:D_net(i)) = histo_acu(:D_net(i)) + 1
            end do
            histo = histo/sum(histo)
            histo_acu = histo_acu/sum(histo_acu)
      end subroutine get_degree_histogram

      subroutine get_knn(knn)
            implicit none
            real*8,intent(out) :: knn(D_max)
            integer            :: i,j

            knn = 0.d0
            do i=1,N_net
                  do j=P_ini(i),P_fin(i)
                        knn(D_net(i)) = knn(D_net(i)) + &
                        D_net(V_net(j))/dble(D_net(i)*V_net(j))
                  end do
            end do
            knn = knn/(avgD2/avgD)

      end subroutine get_knn


end module network