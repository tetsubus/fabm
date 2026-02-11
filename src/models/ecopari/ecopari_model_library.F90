#include "fabm_driver.h"

module ecopari_model_library
   use fabm_types

   use lt_module_ecopelagic
   use lt_module_ecoparam, only: econame
   use lt_eco_fabm_adapter, only: lt_eco_scpelg_fabm

   implicit none
   private

   ! ---- factory (GOTM方式) ----
   type, extends(type_base_model_factory) :: type_factory
   contains
      procedure :: create
   end type

   type(type_factory), save, target, public :: ecopari_model_factory

   type, extends(type_base_model), public :: t_ecopari
      ! 修正1: allocatable に変更
      type(type_state_variable_id), allocatable :: id_state(:)
      type(type_dependency_id)     :: id_tt, id_ss, id_kext
   contains
      procedure :: initialize
      procedure :: do
   end type

   ! --- ChatGPTの助言に基づき、factoryのpublic宣言と関数本体を削除 ---

contains

   subroutine create(self, name, model)
      class(type_factory), intent(in) :: self
      character(*),         intent(in) :: name
      class(type_base_model), pointer :: model

      select case (name)
         case ('pelagic')
            allocate(t_ecopari::model)
      end select
   end subroutine create

   subroutine initialize(self, configunit)
      class(t_ecopari), intent(inout), target :: self
      integer, intent(in) :: configunit
      integer :: i

      ! 修正2: ここで配列を確保
      allocate(self%id_state(npelagic))

      do i=1,npelagic
         call self%register_state_variable(self%id_state(i), trim(econame(i)), '-', trim(econame(i)), 0.0_rk)
      end do

      call self%register_dependency(self%id_tt,   'temperature', 'degC', 'temperature')
      call self%register_dependency(self%id_ss,   'salinity',    'PSU',  'salinity')
      call self%register_dependency(self%id_kext, 'kext',        'm-1',  'light extinction')

   end subroutine initialize

   subroutine do(self, _ARGUMENTS_DO_)
      class(t_ecopari), intent(in) :: self
      _DECLARE_ARGUMENTS_DO_

      integer :: i, ierror
      real(rk) :: tt, ss, kext
      real(rk) :: c(npelagic)
      real(rk) :: rhs(npelagic)
      real(rk) :: tt_a(1), ss_a(1), kext_a(1)
      real(rk) :: ccpl_a(npelagic,1), scpl_a(npelagic,1)
      
      ! 修正3: error #6633, #6638（引数の型・属性不一致）対策
      ! INTENT(OUT/INOUT)に定数を渡せないため、ダミー変数を用意
      real(rk) :: dummy_real_a(npelagic,1)
      real(rk) :: dummy_real_1(1)
      integer  :: dummy_int_1(1)
      real(rk) :: dt_dummy

      dt_dummy = 0.0_rk
      dummy_real_a = 0.0_rk
      dummy_real_1 = 0.0_rk
      dummy_int_1  = 1

      _LOOP_BEGIN_
         _GET_(self%id_tt,tt)
         _GET_(self%id_ss,ss)
         _GET_(self%id_kext,kext)

         do i=1,npelagic
            _GET_(self%id_state(i), c(i))
         end do

         tt_a(1)   = tt
         ss_a(1)   = ss
         kext_a(1) = kext
         do i=1,npelagic
            ccpl_a(i,1) = c(i)
         end do

         ! アダプター呼び出し：すべての引数に「変数」を渡す
         call lt_eco_scpelg_fabm( &
            1, 1, 1, 1, dt_dummy, &
            dummy_real_1, dummy_real_1, dummy_int_1, dummy_int_1, dummy_real_1, dummy_real_1, dummy_real_1, &
            tt_a, ss_a, kext_a, &
            ccpl_a, dummy_real_a, dummy_real_a, scpl_a, dummy_real_a, tt_a, dummy_int_1, &
            dummy_real_1, dummy_real_1, dummy_real_1, dummy_real_1, &
            ierror)

         do i=1,npelagic
            rhs(i) = scpl_a(i,1)
         end do

         do i=1,npelagic
            _ADD_SOURCE_(self%id_state(i), rhs(i))
         end do
      _LOOP_END_
   end subroutine do

end module ecopari_model_library
