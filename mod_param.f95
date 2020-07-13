MODULE MOD_PARAM
	!###########################################################
	IMPLICIT NONE
    INTEGER, PARAMETER		 :: DP=SELECTED_REAL_KIND(15)
    REAL(KIND=DP), PARAMETER :: PI=3.141592653589793_DP
	
	
	!###########################################################
	TYPE MODEL
		REAL(KIND=DP)			  :: XMAX,ZMAX, DH
		INTEGER					  :: NX,NZ
		REAL(KIND=DP),DIMENSION(:,:),ALLOCATABLE:: VP,VS,RHO,QP,QS 			
		REAL(KIND=DP),DIMENSION(:,:),ALLOCATABLE:: 	VPXH,VSXH,RHOXH,QPXH,QSXH
		REAL(KIND=DP),DIMENSION(:,:),ALLOCATABLE:: 	VPZH,VSZH,RHOZH,QPZH,QSZH
	END TYPE MODEL	
	
	
	!###########################################################
	TYPE SOURCE
		INTEGER						:: 	NT
		REAL(KIND=DP)				::	T0, F0,DT,T
		REAL(KIND=DP), ALLOCATABLE	:: 	SIG(:)
		CHARACTER(LEN=:), ALLOCATABLE	:: 	NAME
	END TYPE SOURCE

	
	!###########################################################	
	TYPE BOUNDARY_PML
		INTEGER		:: NAB
		CHARACTER(LEN=:), ALLOCATABLE :: TOPBC
        REAL(KIND=DP),DIMENSION(:,:),ALLOCATABLE ::SX,SZ,SXH,SZH
	END TYPE BOUNDARY_PML

	
	!###########################################################
	
	TYPE GEOMETRY_SNR
		REAL(KIND=DP),ALLOCATABLE, DIMENSION(:)	:: SX, SZ, RX,RZ
		INTEGER,ALLOCATABLE, DIMENSION(:)		:: SXN, SZN,RXN,RZN
		INTEGER									:: NSRC, NREC
		CHARACTER(LEN=:), ALLOCATABLE			:: NAME
	END TYPE GEOMETRY_SNR
	
		
END MODULE MOD_PARAM





