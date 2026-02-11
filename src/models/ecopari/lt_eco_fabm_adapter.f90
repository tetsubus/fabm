module lt_eco_fabm_adapter
  ! FABMの精度定義を使用
  use fabm_types, only: rk
  implicit none
contains

  subroutine lt_eco_scpelg_fabm( &
      ncube,mx,my,mz,dt,zc, &
      wdsp,ktopn,kbtm,hhn,zbtm,dhpn, &
      tt,ss,kext, &
      ccpl,ceg,cfish,scpl,ccbt,ttbt,jcbt, &
      eco_avrg,eco_intg,ribtm,benthic_bivalve_flux, &
      ierror)

    ! 修正2: lt_module_ecopelagic の use を削除（外部参照として解決）
    implicit none

    ! ==== 引数の定義 ====
    integer,intent(in) :: ncube,mx,my,mz
    real(rk),intent(in) :: dt
    real(rk),intent(in) :: zc(:)
    real(rk),intent(in) :: wdsp(:)
    integer,intent(in) :: ktopn(:),kbtm(:)
    real(rk),intent(in) :: hhn(:),zbtm(:),dhpn(:)
    real(rk),intent(in) :: tt(:),ss(:),kext(:)

    real(rk),intent(in)    :: ccpl(:,:)
    real(rk),intent(inout) :: ceg(:,:),cfish(:,:)
    real(rk),intent(out)   :: scpl(:,:)
    real(rk),intent(inout) :: ccbt(:,:),ttbt(:)
    integer,intent(inout) :: jcbt(:)

    real(rk),intent(inout) :: eco_avrg(:),eco_intg(:)
    ! 修正1: intent(inout) に変更
    real(rk),intent(in)    :: ribtm(:)
    real(rk),intent(inout) :: benthic_bivalve_flux(:)
    integer,intent(out)   :: ierror

    ! 実際の EcoPARI ルーチンを呼び出す
    ! (外部サブルーチンとしてリンク時に結合されます)
    call lt_eco_scpelg( &
      ncube,mx,my,mz,dt,zc, &
      wdsp,ktopn,kbtm,hhn,zbtm,dhpn, &
      tt,ss,kext, &
      ccpl,ceg,cfish,scpl,ccbt,ttbt,jcbt, &
      eco_avrg,eco_intg,ribtm,benthic_bivalve_flux, &
      ierror)

  end subroutine lt_eco_scpelg_fabm

end module lt_eco_fabm_adapter
