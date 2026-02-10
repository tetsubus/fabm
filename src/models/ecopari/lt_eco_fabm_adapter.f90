module lt_eco_fabm_adapter
  implicit none
contains

  subroutine lt_eco_scpelg_fabm( &
      ncube,mx,my,mz,dt,zc, &
      wdsp,ktopn,kbtm,hhn,zbtm,dhpn, &
      tt,ss,kext, &
      ccpl,ceg,cfish,scpl,ccbt,ttbt,jcbt, &
      eco_avrg,eco_intg,ribtm,benthic_bivalve_flux, &
      ierror)

    implicit none

    ! ==== 引数（そのまま中継する） ====
    integer,intent(in) :: ncube,mx,my,mz
    real,intent(in)    :: dt
    real,intent(in)    :: zc(:)
    real,intent(in)    :: wdsp(:)
    integer,intent(in) :: ktopn(:),kbtm(:)
    real,intent(in)    :: hhn(:),zbtm(:),dhpn(:)
    real,intent(in)    :: tt(:),ss(:),kext(:)

    real,intent(in)    :: ccpl(:,:)
    real,intent(inout) :: ceg(:,:),cfish(:,:)
    real,intent(out)   :: scpl(:,:)
    real,intent(inout) :: ccbt(:,:),ttbt(:)
    integer,intent(inout) :: jcbt(:)

    real,intent(inout) :: eco_avrg(:),eco_intg(:)
    real,intent(in)    :: ribtm(:)
    real,intent(inout) :: benthic_bivalve_flux(:)

    integer,intent(out) :: ierror

    ! ==== 本体呼び出し ====
    call lt_eco_scpelg( &
      ncube,mx,my,mz,dt,zc, &
      wdsp,ktopn,kbtm,hhn,zbtm,dhpn, &
      tt,ss,kext, &
      ccpl,ceg,cfish,scpl,ccbt,ttbt,jcbt, &
      eco_avrg,eco_intg,ribtm,benthic_bivalve_flux,ierror)

  end subroutine lt_eco_scpelg_fabm

end module lt_eco_fabm_adapter

