module pardif
  implicit none
  integer, parameter :: float64=8
!=======================================================================
!                              pardif.h
!=======================================================================
    ! common /flxdif/ rngdtg, rngdtc, rncdtg, rncdtc, &
    ! hgdtg,  hgdtc,  hgdth,  hcdtg,  hcdtc,  hcdth, &
    ! egdtg,  egdtc,  egdqm,  ecdtg,  ecdtc,  ecdqm, &
    ! deadtc, deadtg, demdqm, deadqm, aag, aac, aam, bbg, bbc, bbm
  real (kind=float64) :: rngdtg
  real (kind=float64) :: rngdtc
  real (kind=float64) :: rncdtg
  real (kind=float64) :: rncdtc
  real (kind=float64) :: hgdtg
  real (kind=float64) :: hgdtc
  real (kind=float64) :: hgdth
  real (kind=float64) :: hcdtg
  real (kind=float64) :: hcdtc
  real (kind=float64) :: hcdth
  real (kind=float64) :: egdtg
  real (kind=float64) :: egdtc
  real (kind=float64) :: egdqm
  real (kind=float64) :: ecdtg
  real (kind=float64) :: ecdtc
  real (kind=float64) :: ecdqm
  real (kind=float64) :: deadtc
  real (kind=float64) :: deadtg
  real (kind=float64) :: demdqm
  real (kind=float64) :: deadqm
  real (kind=float64) :: aag
  real (kind=float64) :: aac
  real (kind=float64) :: aam
  real (kind=float64) :: bbg
  real (kind=float64) :: bbc
  real (kind=float64) :: bbm
end module pardif
