MODULE MOD_CALCULATE
CONTAINS

SUBROUTINE CALCULATE_ACOUSTIC1(M,BC,S,G,SI,PLOTON,NFIG)
	USE MOD_PARAM
    USE MOD_DERIVATIVE;
    USE MOD_READ_WRITE
    USE MOD_UTILITIES;
        
    IMPLICIT NONE
    !============================================================================================
    TYPE(MODEL), INTENT(IN)			:: M			! MODEL, SUITABLE DIMENSION TO APPLY PML
    TYPE(BOUNDARY_PML), INTENT(IN)	:: BC			! COEFF OF PML
	TYPE(SOURCE),INTENT(IN)			:: S			! SOURCE SIGNATURE ETC
    TYPE(GEOMETRY_SNR)				:: G			! GEOMETRY OF SRC
    INTEGER, INTENT(IN)				:: SI			! SOURCE INDEX
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)        :: PLOTON
    INTEGER,OPTIONAL,INTENT(IN)        		:: NFIG
    
	REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)   :: P0,P1,P2        ! FIELD VARIABLES
	REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)   :: DER1,DER2          ! MATRICES FOR TEMP DERIVATIVES
	REAL(KIND=DP),ALLOCATABLE,DIMENSION(:,:)    ::  SS
	
    INTEGER :: D1,D2                        ! FOR DIMENSION OF MATRICES
    INTEGER :: IT,SX,SZ,STEP                         ! FOR ITERAITON NO OVER TIME, & SOURCE POSITION (X,Z)
	!---------------------------------------------
	
	PRINT*, 'WORK LEFT: SOURCE POS, REC POS'
	
	IF(PRESENT(NFIG))THEN
       STEP=NFIG
      ELSE
       STEP=20
    ENDIF    
    D1=m%NZ;        D2=M%NX		!PRINT*, 'FLAG: ', D1,D2
    SZ=D1/2;        SX=D2/2

    ALLOCATE(P0(D1,D2),P1(D1,D2),P2(D1,D2),DER1(D1,D2),DER2(D1,D2), SS(S%NT,D2))
    P0=0.;    P1=0.;    P2=0.;      DER1=0.;    DER2=0.;
    DO IT=1,S%NT
      	WRITE(*,"(A1,A,F10.2,A)",ADVANCE="NO") ACHAR(13), " PERCENT COMPLETED: ", (REAL(IT)/REAL(S%NT))*100.0, "%"
        P1(SZ,SX) = P1(SZ,SX) + S%SIG(IT) 
        CALL DER_2D_CD2O1(DER1,P1,1)
        CALL DER_2D_CD2O1(DER2,P1,2)        
        P2= 2*P1-P0 + ((S%DT/M%DH)**2)*( M%VP**2)*(DER1 +DER2)
        P0=P1;          
        P1=P2;
                 
        IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))THEN
            IF (MOD(IT,STEP)==0)  CALL PLT_MAT(P2,'MODEL','X','Z',1)
             
        END IF  
		SS(IT,:)=P2(4,:)
    END DO

    IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y'))) &
    	&CALL PLT_MAT(SS,'SYNTHETIC SEISMOGRAM','X','NT',2)
    
END SUBROUTINE CALCULATE_ACOUSTIC1


!#####################################################################################

SUBROUTINE CALCULATE_ACOUSTIC2(M,BC,S,G,SI,PLOTON,NFIG)
	USE MOD_PARAM
    USE MOD_DERIVATIVE;
    USE MOD_READ_WRITE
    USE MOD_UTILITIES;
        
    IMPLICIT NONE
    !============================================================================================
    TYPE(MODEL), INTENT(IN)			:: M			! MODEL, SUITABLE DIMENSION TO APPLY PML
    TYPE(BOUNDARY_PML), INTENT(IN)	:: BC			! COEFF OF PML
	TYPE(SOURCE),INTENT(IN)			:: S			! SOURCE SIGNATURE ETC
    TYPE(GEOMETRY_SNR)				:: G			! GEOMETRY OF SRC
    INTEGER, INTENT(IN)				:: SI			! SOURCE INDEX
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)      :: PLOTON
    INTEGER,OPTIONAL        		:: NFIG
    
	REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:) :: P,VX,VZ		 ! FIELD VARIABLES
	REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:) :: DER1,DER2       ! MATRICES FOR TEMP DERIVATIVES
	REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:) :: CONST1,CONST2
	REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)  ::  SS
	
    INTEGER :: D1,D2              ! FOR DIMENSION OF MATRICES
    INTEGER :: IT,SX,SZ,STEP      ! FOR ITERAITON NO OVER TIME, & SOURCE POSITION (X,Z)
	!---------------------------------------------
	
    PRINT*, 'WORK LEFT: SOURCE POS, REC POS'
    	
	IF(PRESENT(NFIG))THEN
       STEP=NFIG
      ELSE
       STEP=20
    ENDIF    
        
    D1=m%NZ;        D2=M%NX
    SZ=D1/2;        SX=D2/2  
    
    ALLOCATE(P(D1,D2),    VX(D1,D2),   VZ(D1,D2));		
    ALLOCATE(DER1(D1,D2), DER2(D1,D2), SS(S%NT,D2));
    
    P=0.;    VX=0.;    VZ=0.;
    DER1=0.;    DER2=0.;

	CONST1=(S%DT/(M%DH*M%RHO))
    CONST2=  M%RHO * (M%VP**2) * (S%DT/M%DH)
    
    DO IT=1,S%NT
        WRITE(*,"(A1,A,F10.2,A)",ADVANCE="NO") ACHAR(13), " PERCENT COMPLETED: ", (REAL(IT)/REAL(S%NT))*100.0, "%"
        P(SZ,SX) = P(SZ,SX) + S%SIG(IT) 
                
        CALL DER_2D_FD1O1(DER1,P,1)
		VZ = VZ + CONST1*(DER1);     !CALL PRINT_MAT('VZ MATRIX IS ',100000*VZ)

        CALL DER_2D_FD1O1(DER2,P,2)
		VX = VX + CONST1*(DER2);     !CALL PRINT_MAT('VX MATRIX IS ',100000*VX) 
    
		CALL DER_2D_BD1O1(DER1,VX,2)
        CALL DER_2D_BD1O1(DER2,VZ,1)        
        P= P + CONST2*(DER1 +DER2);  !CALL PRINT_MAT('P MATRIX IS ',P)

        IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))THEN
            IF (MOD(IT,STEP)==0)THEN
                CALL PLT_MAT(P,'PRESSURE WAVEFIELD','X','Z',2)
            END IF
        END IF         
		SS(IT,:)=P(4,:)
    END DO
    
    IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))&
        &CALL PLT_MAT(SS,'SYNTHETIC SEISMOGRAM','X','NT',10)

END SUBROUTINE CALCULATE_ACOUSTIC2



!#####################################################################################
SUBROUTINE CALCULATE_ACOUSTIC_PML(M,BC,S,G,SI,PLOTON,NFIG)
    USE MOD_PARAM
    USE MOD_DERIVATIVE;
    USE MOD_READ_WRITE
    USE MOD_UTILITIES;
        
    IMPLICIT NONE
    !============================================================================================
    TYPE(MODEL), INTENT(IN)         :: M            ! MODEL, SUITABLE DIMENSION TO APPLY PML
    TYPE(BOUNDARY_PML), INTENT(INOUT)  :: BC           ! COEFF OF PML
    TYPE(SOURCE),INTENT(IN)         :: S            ! SOURCE SIGNATURE ETC
    TYPE(GEOMETRY_SNR)              :: G            ! GEOMETRY OF SRC
    INTEGER, INTENT(IN)             :: SI           ! SOURCE INDEX
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)      :: PLOTON
    INTEGER,OPTIONAL        		:: NFIG
    
    REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)   :: AX,AXH,AZ,AZH, BX,BXH,BZ,BZH ! MATRICES FOR COEFF
    REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)   :: VX,VZ,PX,PZ,P        ! FIELD VARIABLES
    
    REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)   :: DER1,DER2          ! MATRICES FOR TEMP DERIVATIVES
    INTEGER :: D1,D2                        ! FOR DIMENSION OF MATRICES
    INTEGER :: IT,SX,SZ,STEP                         ! FOR ITERAITON NO OVER TIME, & SOURCE POSITION (X,Z)
    
    REAL(KIND=DP),ALLOCATABLE,DIMENSION(:,:)    ::  SS

    !============================================================================================       
	PRINT*, 'WORK LEFT: SOURCE POS, REC POS'
    	
	IF(PRESENT(NFIG))THEN
       STEP=NFIG
      ELSE
       STEP=20
    ENDIF

    D1=m%NZ;        D2=M%NX;	  PRINT*, 'FLAG: ', D1,D2

    ALLOCATE(AX(D1,D2),AXH(D1,D2),AZ(D1,D2),AZH(D1,D2))
    ALLOCATE(BX(D1,D2),BXH(D1,D2),BZ(D1,D2),BZH(D1,D2))
    ALLOCATE(VX(D1,D2),VZ(D1,D2),PX(D1,D2),PZ(D1,D2),P(D1,D2))
    VX=0.;      VZ=0.;    P=0.;       PX=0.;      PZ=0.
    
    ALLOCATE(DER1(D1,D2));          DER1=0.;   
    ALLOCATE(SS(S%NT,D2));		    SS=0.
    
     	        
    AXH=(1.0/S%DT - BC%SXH/2.0)  /  (  1.0/S%DT + BC%SXH/2.0 );	    !CALL PLT_MAT(AXH,'AXH MAT','X','Z',5)    !COEFF TO UPDATE VX  
    AZH=(1.0/S%DT - BC%SZH/2.0)  /  (  1.0/S%DT + BC%SZH/2.0 );     !CALL PLT_MAT(AZH,'AZH MAT','X','Z',5) 	 !COEFF TO UPDATE VZ
	AX =(1.0/S%DT - BC%SX/2.0 )  /  (  1.0/S%DT + BC%SX/2.0  );		!CALL PLT_MAT(AX,'AX MAT','X','Z',5)		 !COEFF TO UPDATE PX
    AZ =(1.0/S%DT - BC%SZ/2.0 )  /  (  1.0/S%DT + BC%SZ/2.0  );		!CALL PLT_MAT(AZ,'AZ MAT','X','Z',5)		 !COEFF TO UPDATE PZ
 
    BXH= 1.0/ (  M%RHOXH * M%DH *(1.0/S%DT + BC%SXH/2.0)  );   !CALL PLT_MAT(BXH,'BXH MAT','X','Z',5)    !COEFF TO UPDATE VX
    BZH= 1.0/ (  M%RHOZH * M%DH *(1.0/S%DT + BC%SZH/2.0)  );   !CALL PLT_MAT(BZH,'BZH MAT','X','Z',5)    !COEFF TO UPDATE VZ
    BX = M%RHO*(M%VP**2)  /  (  M%DH*(1.0/S%DT + BC%SX/2.0) ); !CALL PLT_MAT(BX,'BX MAT','X','Z',5)     !COEFF TO UPDATE PX
    BZ = M%RHO*(M%VP**2)  /  (  M%DH*(1.0/S%DT + BC%SZ/2.0) ); !CALL PLT_MAT(BZ,'BZ MAT','X','Z',5)     !COEFF TO UPDATE  PZ

    SZ=(D1-BC%NAB)/2;     SX=D2/2 ; 
	
    DO IT=1,S%NT
      
        WRITE(*,"(A1,A,F10.2,A)",ADVANCE="NO") ACHAR(13), " PERCENT COMPLETED: ", (REAL(IT)/REAL(S%NT))*100.0, "%"
        
		P(SZ,SX) = P(SZ,SX) + S%SIG(IT)
        
        CALL DER_2D_FD1O1(DER1,P,2)     !DER1(:,1:(D2-1))=  P(:,2:D2)-P(:,1: (D2-1)) 
        VX(:,:)= AXH*VX  +  BXH*DER1;		!CALL PLT_MAT(VX,'VX MAT','X','Z',2)
        
        CALL DER_2D_FD1O1(DER1,P,1)     !DER1(1:(D1-1),:)=  P(2:D1,:)-P(1:(D1-1),:)  
        VZ(:,:)= AZH*VZ  +  BZH*DER1;      !CALL PLT_MAT(VZ,'VZ MAT','X','Z',2)
        
        CALL DER_2D_BD1O1(DER1,VX,2)    !DER1(:,1:D2-1)=VX(:,2:D2)-VX(:,1:D2-1)
        PX(:,:)= AX*PX  +  BX*DER1;        !CALL PLT_MAT(PX,'PX MAT','X','Z',2)

        CALL DER_2D_BD1O1(DER1,VZ,1)    !DER1(1:D1-1,:)=VZ(2:D1,:)-VZ(1:D1-1,:)
        PZ(:,:)= AZ*PZ  +  BZ*DER1;        !CALL PLT_MAT(PZ,'PZ MAT','X','Z',2)
        
        P= PX + PZ
		
        IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))THEN
            IF (MOD(IT,STEP)==0)THEN
                CALL PLT_MAT(P,'PRESSURE WAVEFIELD','X','Z',4)
            END IF
        END IF
        
        SS(IT,:)= P(10,:)
        
    END DO  
    
    IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))&
        &CALL PLT_MAT(SS,'SYNTHETIC SEISMOGRAM','X','NT',10) 
END SUBROUTINE CALCULATE_ACOUSTIC_PML
!#####################################################################################


SUBROUTINE CALCULATE_ACOUSTIC_PML_FD_LELE(M,BC,S,G,SI,PLOTON,NFIG)
    USE MOD_PARAM
    USE MOD_DERIVATIVE_COMPACT;
    USE MOD_READ_WRITE
    USE MOD_UTILITIES;
        
    IMPLICIT NONE
    !============================================================================================
    TYPE(MODEL), INTENT(IN)         :: M            ! MODEL, SUITABLE DIMENSION TO APPLY PML
    TYPE(BOUNDARY_PML), INTENT(INOUT)  :: BC           ! COEFF OF PML
    TYPE(SOURCE),INTENT(IN)         :: S            ! SOURCE SIGNATURE ETC
    TYPE(GEOMETRY_SNR)              :: G            ! GEOMETRY OF SRC
    INTEGER, INTENT(IN)             :: SI           ! SOURCE INDEX
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)      :: PLOTON
    INTEGER,OPTIONAL        		:: NFIG
    
    REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)   :: AX,AXH,AZ,AZH, BX,BXH,BZ,BZH ! MATRICES FOR COEFF
    REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)   :: VX,VZ,PX,PZ,P        ! FIELD VARIABLES
    
    REAL(KIND=DP),ALLOCATABLE, DIMENSION(:,:)   :: DER1,DER2          ! MATRICES FOR TEMP DERIVATIVES
    INTEGER :: D1,D2                        ! FOR DIMENSION OF MATRICES
    INTEGER :: IT,SX,SZ,STEP                         ! FOR ITERAITON NO OVER TIME, & SOURCE POSITION (X,Z)
    
    REAL(KIND=DP),ALLOCATABLE,DIMENSION(:,:)    ::  SS

    !============================================================================================       
	PRINT*, 'WORK LEFT: SOURCE POS, REC POS'
    	
	IF(PRESENT(NFIG))THEN
       STEP=NFIG
      ELSE
       STEP=20
    ENDIF

    D1=m%NZ;        D2=M%NX;	  PRINT*, 'FLAG: ', D1,D2

    ALLOCATE(AX(D1,D2),AXH(D1,D2),AZ(D1,D2),AZH(D1,D2))
    ALLOCATE(BX(D1,D2),BXH(D1,D2),BZ(D1,D2),BZH(D1,D2))
    ALLOCATE(VX(D1,D2),VZ(D1,D2),PX(D1,D2),PZ(D1,D2),P(D1,D2))
    VX=0.;      VZ=0.;    P=0.;       PX=0.;      PZ=0.
    
    ALLOCATE(DER1(D1,D2));          DER1=0.;   
    ALLOCATE(SS(S%NT,D2));		    SS=0.  
     	        
    AXH=(1.0/S%DT - BC%SXH/2.0)  /  (  1.0/S%DT + BC%SXH/2.0 );	    !CALL PLT_MAT(AXH,'AXH MAT','X','Z',5)    !COEFF TO UPDATE VX  
    AZH=(1.0/S%DT - BC%SZH/2.0)  /  (  1.0/S%DT + BC%SZH/2.0 );     !CALL PLT_MAT(AZH,'AZH MAT','X','Z',5) 	 !COEFF TO UPDATE VZ
	AX =(1.0/S%DT - BC%SX/2.0 )  /  (  1.0/S%DT + BC%SX/2.0  );		!CALL PLT_MAT(AX,'AX MAT','X','Z',5)		 !COEFF TO UPDATE PX
    AZ =(1.0/S%DT - BC%SZ/2.0 )  /  (  1.0/S%DT + BC%SZ/2.0  );		!CALL PLT_MAT(AZ,'AZ MAT','X','Z',5)		 !COEFF TO UPDATE PZ
 
    BXH= 1.0/ (  M%RHOXH * M%DH *(1.0/S%DT + BC%SXH/2.0)  );   !CALL PLT_MAT(BXH,'BXH MAT','X','Z',5)    !COEFF TO UPDATE VX
    BZH= 1.0/ (  M%RHOZH * M%DH *(1.0/S%DT + BC%SZH/2.0)  );   !CALL PLT_MAT(BZH,'BZH MAT','X','Z',5)    !COEFF TO UPDATE VZ
    BX = M%RHO*(M%VP**2)  /  (  M%DH*(1.0/S%DT + BC%SX/2.0) ); !CALL PLT_MAT(BX,'BX MAT','X','Z',5)     !COEFF TO UPDATE PX
    BZ = M%RHO*(M%VP**2)  /  (  M%DH*(1.0/S%DT + BC%SZ/2.0) ); !CALL PLT_MAT(BZ,'BZ MAT','X','Z',5)     !COEFF TO UPDATE  PZ

    SZ=(D1-BC%NAB)/2;     SX=D2/2 ; 

    CALL DER_COMPACT_2D_CD1O6(DER1,P,2)

!$$$$$$     DO IT=1,S%NT
!$$$$$$       
!$$$$$$         WRITE(*,"(A1,A,F10.2,A)",ADVANCE="NO") ACHAR(13), " PERCENT COMPLETED: ", (REAL(IT)/REAL(S%NT))*100.0, "%"
!$$$$$$         
!$$$$$$         P(SZ,SX) = P(SZ,SX) + S%SIG(IT)
!$$$$$$         
!$$$$$$         CALL DER_2D_FD1O1(DER1,P,2)     !DER1(:,1:(D2-1))=  P(:,2:D2)-P(:,1: (D2-1)) 
!$$$$$$         VX(:,:)= AXH*VX  +  BXH*DER1;       !CALL PLT_MAT(VX,'VX MAT','X','Z',2)
!$$$$$$         
!$$$$$$         CALL DER_2D_FD1O1(DER1,P,1)     !DER1(1:(D1-1),:)=  P(2:D1,:)-P(1:(D1-1),:)  
!$$$$$$         VZ(:,:)= AZH*VZ  +  BZH*DER1;      !CALL PLT_MAT(VZ,'VZ MAT','X','Z',2)
!$$$$$$         
!$$$$$$         CALL DER_2D_BD1O1(DER1,VX,2)    !DER1(:,1:D2-1)=VX(:,2:D2)-VX(:,1:D2-1)
!$$$$$$         PX(:,:)= AX*PX  +  BX*DER1;        !CALL PLT_MAT(PX,'PX MAT','X','Z',2)
!$$$$$$ 
!$$$$$$         CALL DER_2D_BD1O1(DER1,VZ,1)    !DER1(1:D1-1,:)=VZ(2:D1,:)-VZ(1:D1-1,:)
!$$$$$$         PZ(:,:)= AZ*PZ  +  BZ*DER1;        !CALL PLT_MAT(PZ,'PZ MAT','X','Z',2)
!$$$$$$         
!$$$$$$         P= PX + PZ
!$$$$$$         
!$$$$$$         IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))THEN
!$$$$$$             IF (MOD(IT,STEP)==0)THEN
!$$$$$$                 CALL PLT_MAT(P,'PRESSURE WAVEFIELD','X','Z',4)
!$$$$$$             END IF
!$$$$$$         END IF
!$$$$$$         
!$$$$$$         SS(IT,:)= P(10,:)
!$$$$$$         
!$$$$$$     END DO  
!$$$$$$     
!$$$$$$     IF(PRESENT(PLOTON).AND.((PLOTON=='Y').OR.(PLOTON=='y')))&
!$$$$$$         &CALL PLT_MAT(SS,'SYNTHETIC SEISMOGRAM','X','NT',10) 
        
END SUBROUTINE CALCULATE_ACOUSTIC_PML_FD_LELE
!#####################################################################################


END MODULE MOD_CALCULATE