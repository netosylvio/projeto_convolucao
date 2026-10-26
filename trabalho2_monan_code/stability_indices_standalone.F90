! 1. modulo de tipos de precisão
! **********
module mpas_kind_types
  implicit none
  integer, parameter :: RKIND = 8  ! Precisao dupla (real*8)
end module mpas_kind_types

! 2. modulo de indices de estabilidade (codigo-MONAN)
! **********
MODULE stability_indices
  use mpas_kind_types, only : RKIND
  implicit none

  private
  public :: INDX1

CONTAINS

 SUBROUTINE INDX1 (JNO, P, TS, TSD, RLI, RKI, RWI)
     implicit none

     integer, intent(in) :: JNO
     integer :: I, J, K, IHI

     real (kind=RKIND), dimension(JNO), intent(in) :: P, TS, TSD
     real (kind=RKIND), dimension(JNO) :: TW
     real (kind=RKIND) :: PL(3), TL(3), TDL(3)
     real (kind=RKIND), intent(out) :: RLI, RKI, RWI

     real (kind=RKIND) :: TC, TH, WTH, WTC, WSUM, THSUM, P1, P2, PFINISH
     real (kind=RKIND) :: PLOG1, PLOG2, PLOG3, PLOG4
     real (kind=RKIND) :: DP1, E2, FACTOR, FACTORD, FACTORT, PPARCEL, TDE1, TDE2, TDPARCEL
     real (kind=RKIND) :: TE1, TE2, TH1, TH2, THAVG, THW, TP, TP500, TPARCEL, W, W1, W2, WAVEG
     real (kind=RKIND) :: WAVG, WFW, X

     IHI = 0
     PL(1) = 850.0_RKIND
     PL(2) = 700.0_RKIND
     PL(3) = 500.0_RKIND

     do K = 1, JNO
        TC = TCONOF(TS(K), TSD(K))
        TH = THETA(TS(K)+273.16_RKIND, 1000.0_RKIND, P(K)) - 273.16_RKIND
        WTH = WOBF(TH)
        WTC = WOBF(TC)
        TW(K) = TH - WTH + WTC
     end do

     DP1 = 50.0_RKIND
     WSUM = 0.0_RKIND
     THSUM = 0.0_RKIND
     J = 1
     P1 = P(1)
     TE1 = TS(1)
     TDE1 = TSD(1)
     TH1 = THETA(TE1+273.16_RKIND, 1000.0_RKIND, P(1))
     W1 = WMROF(P(1), TDE1)
     PFINISH = P1 - DP1

 3   P2 = PFINISH
     if (P(J+1) - P2 .ge. 0.0_RKIND) then
        P2 = P(J+1)
        J = J + 1
     end if

     PLOG1 = LOG(P(J) / P(J+1))
     FACTORT = (TS(J) - TS(J+1)) / PLOG1
     FACTORD = (TSD(J) - TSD(J+1)) / PLOG1
     PLOG2 = LOG(P2 / P(J+1))
     TE2 = TS(J+1) + FACTORT * PLOG2
     TDE2 = TSD(J+1) + FACTORD * PLOG2
     TH2 = THETA(TE2+273.16_RKIND, 1000.0_RKIND, P2)
     W2 = WMROF(P2, TDE2)
     PLOG3 = LOG(P1 / P2)
     TH = 0.5_RKIND * (TH1 + TH2) * PLOG3
     W = 0.5_RKIND * (W1 + W2) * PLOG3
     THSUM = THSUM + TH
     WSUM = WSUM + W
     P1 = P2
     TH1 = TH2
     W1 = W2
     if (P2 .gt. PFINISH) goto 3

     PLOG4 = LOG(P(1) / PFINISH)
     THAVG = THSUM / PLOG4
     WAVG = WSUM / PLOG4
     PPARCEL = P(1) - 0.5_RKIND * DP1
     TPARCEL = THETA(THAVG, PPARCEL, 1000.0_RKIND) - 273.16_RKIND
     X = 0.0200_RKIND * (TPARCEL - 12.5_RKIND + 7500.0_RKIND / PPARCEL)
     WFW = 1.0_RKIND + 0.0000045_RKIND * PPARCEL + 0.00140_RKIND * X * X
     E2 = 0.001_RKIND * WAVG * PPARCEL / ((WAVG * 0.001_RKIND + 0.62197_RKIND) * WFW)
     TDPARCEL = DPTOF(E2)
     TC = TCONOF(TPARCEL, TDPARCEL)
     TH = THAVG - 273.16_RKIND
     WTH = WOBF(TH)
     WTC = WOBF(TC)
     THW = TH - WTH + WTC
     TP500 = SATLFT(THW, 500.0_RKIND)

     do J = 1, 3
        do I = 1, JNO
           if (PL(J) - P(I) .eq. 0.0_RKIND) then
              TL(J) = TS(I)
              TDL(J) = TSD(I)
              exit
           else if (PL(J) - P(I) .gt. 0.0_RKIND) then
              if (J .eq. 1 .and. I .eq. 1) then
                 IHI = 1
                 exit
              end if
              FACTOR = LOG(PL(J) / P(I)) / LOG(P(I-1) / P(I))
              TL(J) = TS(I) + FACTOR * (TS(I-1) - TS(I))
              TDL(J) = TSD(I) + FACTOR * (TSD(I-1) - TSD(I))
              exit
           end if
        end do
     end do

     RLI = TL(3) - TP500

     if (IHI .ne. 0) then
        RKI = 9.96921e+36_RKIND
        RWI = 9.96921e+36_RKIND
        return
     end if

     RKI = (TL(1) - TL(3)) + TDL(1) - (TL(2) - TDL(2))

     TC = TCONOF(TL(1), TDL(1))
     TH = THETA(TL(1)+273.16_RKIND, 1000.0_RKIND, 850.0_RKIND) - 273.16_RKIND
     WTH = WOBF(TH)
     WTC = WOBF(TC)
     THW = TH - WTH + WTC
     TP = SATLFT(THW, 500.0_RKIND)
     RWI = TL(3) - TP

     return
 END SUBROUTINE INDX1

 REAL(kind=RKIND) FUNCTION WOBF(T)
     implicit none
     real (kind=RKIND), intent(in) :: T
     real (kind=RKIND) :: X, POL

     X = T - 20.0_RKIND
     if (X .le. 0.0_RKIND) then
        POL = 1.000_RKIND + X * (-8.8416605E-3_RKIND + X * (1.4714143E-4_RKIND + X * (-9.6719890E-7_RKIND &
              + X * (-3.2607217E-8_RKIND + X * (-3.8598073E-10_RKIND)))))
        POL = POL * POL
        WOBF = 15.130_RKIND / (POL * POL)
     else
        POL = 1.000_RKIND + X * (3.6182989E-3_RKIND + X * (-1.3603273E-5_RKIND + X * (4.9618922E-7_RKIND &
              + X * (-6.1059365E-9_RKIND + X * (3.9401551E-11_RKIND + X * (-1.2588129E-13_RKIND &
              + X * (1.6688280E-16_RKIND)))))))
        POL = POL * POL
        WOBF = 29.930_RKIND / (POL * POL) + 0.9600_RKIND * X - 14.800_RKIND
     end if
 END FUNCTION WOBF

 REAL(kind=RKIND) FUNCTION SATLFT (THM, P)
     implicit none
     real (kind=RKIND), intent(in) :: THM, P
     real, parameter :: ROCP = 0.28571428
     real (kind=RKIND) :: PWRP, TONE, TTWO, EONE, ETWO, RATE, EOR

     if (ABS(P - 1000.0_RKIND) .le. 0.0010_RKIND) then
        SATLFT = THM
        return
     end if

     PWRP = (P / 1000.0_RKIND)**ROCP
     TONE = (THM + 273.16_RKIND) * PWRP - 273.16_RKIND
     EONE = WOBF(TONE) - WOBF(THM)
     RATE = 1.0_RKIND
     TTWO = TONE - EONE * RATE

 330 continue
     ETWO = (TTWO + 273.16_RKIND) / PWRP - 273.16_RKIND
     ETWO = ETWO + WOBF(TTWO) - WOBF(ETWO) - THM
     EOR = ETWO * RATE
     if (ABS(EOR) .gt. 0.1000_RKIND) then
        RATE = (TTWO - TONE) / (ETWO - EONE)
        TONE = TTWO
        EONE = ETWO
        TTWO = TONE - EONE * RATE
        goto 330
     end if
     SATLFT = TTWO - EOR
 END FUNCTION SATLFT

 REAL(kind=RKIND) FUNCTION TCONOF(TEMP, DEWPT)
     implicit none
     real (kind=RKIND), intent(in) :: TEMP, DEWPT
     real (kind=RKIND) :: S, T, DLT

     S = TEMP - DEWPT
     T = TEMP
     if (TEMP .gt. 100.0_RKIND) T = TEMP - 273.16_RKIND
     DLT = S * (1.2185_RKIND + 0.001278_RKIND * T + S * (-0.002190_RKIND + 11.73E-6_RKIND * S - 5.20E-6_RKIND * T))
     TCONOF = T - DLT
 END FUNCTION TCONOF

 REAL(kind=RKIND) FUNCTION WMROF(P, TD)
     implicit none
     real (kind=RKIND), intent(in) :: P, TD
     real (kind=RKIND) :: T, X, WFW, FWESW

     T = TD
     if (T .gt. 100.0_RKIND) T = T - 273.16_RKIND
     X = 0.0200_RKIND * (T - 12.5_RKIND + 7500.0_RKIND / P)
     WFW = 1.0_RKIND + 0.0000045_RKIND * P + 0.00140_RKIND * X * X
     FWESW = WFW * VAPFW(T)
     WMROF = 621.97_RKIND * (FWESW / (P - FWESW))
 END FUNCTION WMROF

 REAL(kind=RKIND) FUNCTION THETA(T, P2, P1)
     implicit none
     real (kind=RKIND), intent(in) :: T, P2, P1
     real (kind=RKIND) :: Rd, Cp, Rd_Cp

     Rd = 287.058_RKIND
     Cp = 1004.0_RKIND
     Rd_Cp = Rd / Cp
     THETA = T * (P2 / P1)**Rd_Cp
 END FUNCTION THETA

 REAL(kind=RKIND) FUNCTION DPTOF(EW)
     implicit none
     real (kind=RKIND), intent(in) :: EW
     real (kind=RKIND) :: TOL, X, BOT, DELTM, EDP, DTDE, DELT, DM

     TOL = 0.00010_RKIND
     if (EW .le. 0.21382876E-09_RKIND) then
        DPTOF = -10000.0_RKIND
        return
     end if

     X = LOG(EW / 6.1078_RKIND)
     BOT = 17.269388_RKIND - X
     DPTOF = (237.3_RKIND * X) / BOT
     BOT = BOT * EW
     DELTM = 0.0_RKIND

 200 continue
     EDP = VAPFW(DPTOF)
     DTDE = (DPTOF + 237.3_RKIND) / BOT
     DELT = DTDE * (EW - EDP)
     DPTOF = DPTOF + DELT
     DM = DELT - DELTM
     if (ABS(DM) .lt. 1.E-7_RKIND) TOL = ABS(DELT)
     DELTM = -DELT
     if (ABS(DELT) .gt. TOL) goto 200

     DPTOF = DPTOF - TOL
 END FUNCTION DPTOF

 REAL(kind=RKIND) FUNCTION VAPFW(T)
     implicit none
     real (kind=RKIND), intent(in) :: T
     real (kind=RKIND) :: X, POL

     X = T
     if (X .gt. 100.0_RKIND) X = X - 273.16_RKIND
     POL = 0.99999683E-00_RKIND + X * (-0.90826951E-02_RKIND + X * (0.70736169E-04_RKIND + X * (-0.61117958E-06_RKIND &
           + X * (0.43884187E-08_RKIND + X * (-0.29883885E-10_RKIND + X * (0.21874425E-12_RKIND &
           + X * (-0.17892321E-14_RKIND + X * (0.11112018E-16_RKIND + X * (-0.30994571E-19_RKIND)))))))))
     POL = POL * POL
     POL = POL * POL
     VAPFW = 6.107800_RKIND / (POL * POL)
 END FUNCTION VAPFW

END MODULE stability_indices

! 3. programa principal
! **********
program main_stability
  use mpas_kind_types, only : RKIND
  use stability_indices, only : INDX1
  implicit none

  ! grade e passo de tempo sintéticos (nx e ny=grade horizontal 2D; jno=nives verticais; nsteps=passo de tempo)
  integer, parameter :: nx = 500
  integer, parameter :: ny = 500
  integer, parameter :: jno = 30
  integer, parameter :: nsteps = 100
  ! p=pressao atmosferica; ts=temperatura do ar; tsd=temperatura ponto de orvalho
  real(kind=RKIND), dimension(jno) :: p, ts, tsd
  real(kind=RKIND), dimension(nx, ny) :: rli, rki, rwi
  real :: start_time, end_time
  integer :: i, j, k, step

  ! Inicializacao do perfil atmosferico sintético
  do k = 1, jno
     p(k)   = 1000.0_RKIND - (k - 1) * 25.0_RKIND
     ts(k)  = 25.0_RKIND - (k - 1) * 2.0_RKIND
     tsd(k) = 18.0_RKIND - (k - 1) * 2.5_RKIND
  end do

  call cpu_time(start_time)

! Pre-processador para as 4 versoes a ser utilizado no momento da compilacao
#ifdef VERSION_BASE
  ! 1. Versao Base: i externo, j interno (sem vetorizacao)
  do step = 1, nsteps
     do i = 1, nx
        do j = 1, ny
           call INDX1(jno, p, ts, tsd, rli(i,j), rki(i,j), rwi(i,j))
        end do
     end do
  end do
#endif

#ifdef VERSION_MEMORIA
  ! 2. Versao Memoria: j externo, i interno (Column-Major)
  do step = 1, nsteps
     do j = 1, ny
        do i = 1, nx
           call INDX1(jno, p, ts, tsd, rli(i,j), rki(i,j), rwi(i,j))
        end do
     end do
  end do
#endif

#ifdef VERSION_SIMD
  ! 3. Versao SIMD: i externo, j interno com pragma
  do step = 1, nsteps
     do i = 1, nx
        !$OMP SIMD
        do j = 1, ny
           call INDX1(jno, p, ts, tsd, rli(i,j), rki(i,j), rwi(i,j))
        end do
     end do
  end do
#endif

#ifdef VERSION_COMBINADO
  ! 4. Versao Combinada: j externo, i interno com pragma
  do step = 1, nsteps
     do j = 1, ny
        !$OMP SIMD
        do i = 1, nx
           call INDX1(jno, p, ts, tsd, rli(i,j), rki(i,j), rwi(i,j))
        end do
     end do
  end do
#endif

  call cpu_time(end_time)

  print *, "--------------------------------------------------"
  print *, "Execucao concluida com sucesso!"
  print *, "Tempo total de execucao: ", end_time - start_time, " s"
  print *, "Checksum RLI (Soma Total): ", sum(rli)
  print *, "Checksum RKI (Soma Total): ", sum(rki)
  print *, "Checksum RWI (Soma Total): ", sum(rwi)
  print *, "--------------------------------------------------"

end program main_stability
