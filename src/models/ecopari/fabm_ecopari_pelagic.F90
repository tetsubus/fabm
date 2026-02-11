#include "fabm_driver.h"

module fabm_ecopari_pelagic
   use fabm_types
   use lt_module_ecopelagic
   use lt_module_ecoparam, only: econame
   use lt_eco_fabm_adapter, only: lt_eco_scpelg_fabm

   implicit none
   private

   type, extends(type_base_model), public :: type_ecopari_pelagic
      type(type_state_variable_id) :: id_state(npelagic)
      type(type_dependency_id)     :: id_tt, id_ss, id_kext
   contains
      procedure :: initialize
      procedure :: do
   end type

contains

   subroutine initialize(self, configunit)
      class(type_ecopari_pelagic), intent(inout), target :: self
      integer, intent(in) :: configunit
      integer :: i

      ! ---- state variables ----
      do i=1,npelagic
         call self%register_state_variable(self%id_state(i), trim(econame(i)), '-', trim(econame(i)), 0.0_rk)
      end do

      ! ---- dependencies ----
      call self%register_dependency(self%id_tt,   'temperature', 'degC', 'temperature')
      call self%register_dependency(self%id_ss,   'salinity',    'PSU',  'salinity')
      call self%register_dependency(self%id_kext, 'kext',        'm-1',  'light extinction')

   end subroutine initialize


   subroutine do(self, _ARGUMENTS_DO_)
      class(type_ecopari_pelagic), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_

      integer :: i, ierror
      real(rk) :: tt, ss, kext
      real(rk) :: c(npelagic)
      real(rk) :: rhs(npelagic)

      ! ---- spatial loop ----
      _LOOP_BEGIN_

         ! dependencies
         _GET_(self%id_tt,tt)
         _GET_(self%id_ss,ss)
         _GET_(self%id_kext,kext)

         ! state
         do i=1,npelagic
            _GET_(self%id_state(i), c(i))
         end do

         ! ---- call EcoPARI reaction (cell-wise) ----
         ! ここは adapter のインタフェースが「配列(セル数)」前提なので、
         ! まずは 1セルだけの形で呼ぶために、次ステップで小配列に詰めます。
         !
         ! いまは rhs=0 でコンパイル通す骨格にしておきます。
         rhs = 0.0_rk
         ierror = 0

         ! return rhs (dt を掛けない)
         do i=1,npelagic
            _ADD_SOURCE_(self%id_state(i), rhs(i))
         end do

      _LOOP_END_
   end subroutine do

end module fabm_ecopari_pelagic

