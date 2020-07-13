MODULE MOD_DERIVATIVE
CONTAINS


!######################### DERIVATIVE FOR 2D #############################################
SUBROUTINE DER_2D_FD1O1(DER,A,DIR)
	! 2D (F)ORWARD (D)ERIVATIVE ORDER= 1; OF ACCURACY (O)RDER=1
	USE MOD_PARAM
    REAL(KIND=DP),ALLOCATABLE, INTENT(OUT) :: DER(:,:)
    REAL(KIND=DP), INTENT(IN) 			   :: A(:,:)
	INTEGER,INTENT(IN)					   :: DIR	   !DIRECTION OF DERIVATIVE
	INTEGER :: D1,D2
    
    D1=SIZE(A,1);			    D2=SIZE(A,2)
	ALLOCATE(DER(D1,D2));	    DER=0.

	IF (DIR==1) THEN
	    DER(2:D1-1,:) = A(3:D1,:)-  A(2:D1-1,:)
    ELSE IF (DIR==2) THEN
		DER(:,2:D2-1) =  A(:,3:D2) - A(:,2:D2-1)
    ELSE
      PRINT*, 'WRONG DIMENSION CHOSEN'
    ENDIF

END SUBROUTINE DER_2D_FD1O1


!######################################################################
SUBROUTINE DER_2D_BD1O1(DER,A,DIR)
	! 2D (B)ACKWARD (D)ERIVATIVE ORDER= 1; OF ACCURACY (O)RDER=1
	USE MOD_PARAM
    REAL(KIND=DP),ALLOCATABLE, INTENT(OUT) :: DER(:,:)
    REAL(KIND=DP), INTENT(IN) 			   :: A(:,:)
	INTEGER,INTENT(IN)					   :: DIR	   !DIRECTION OF DERIVATIVE
	INTEGER :: D1,D2
    
    D1=SIZE(A,1);			    D2=SIZE(A,2)
	ALLOCATE(DER(D1,D2));	    DER=0.

	IF (DIR==1) THEN
	    DER(2:D1-1,:) = A(2:D1-1,:)-A(1:D1-2,:)
    ELSE IF (DIR==2) THEN
		DER(:,2:D2-1) = A(:,2:D2-1)-A(:,1:D2-2)
    ELSE
      PRINT*, 'WRONG DIMENSION CHOSEN'
    ENDIF

END SUBROUTINE DER_2D_BD1O1


!######################################################################
SUBROUTINE DER_2D_CD2O1(DER,A,DIR)
	! 2D (C)ENTRAL (D)ERIVATIVE ORDER= 1; OF ACCURACY (O)RDER=1
	USE MOD_PARAM
    REAL(KIND=DP),ALLOCATABLE, INTENT(OUT) :: DER(:,:)
    REAL(KIND=DP), INTENT(IN) 			   :: A(:,:)
	INTEGER,INTENT(IN)					   :: DIR	   !DIRECTION OF DERIVATIVE
	INTEGER :: D1,D2
    
    D1=SIZE(A,1);			    D2=SIZE(A,2)
	ALLOCATE(DER(D1,D2));	    DER=0.

	IF (DIR==1) THEN
	    DER(2:D1-1,:) = A(3:D1,:) -2*A(2:D1-1,:) + A(1:D1-2,:)
    ELSE IF (DIR==2) THEN
		DER(:,2:D2-1) =  A(:,3:D2) -2*A(:,2:D2-1) +A(:,1:D2-2)
    ELSE
      PRINT*, 'WRONG DIMENSION CHOSEN'
    ENDIF

END SUBROUTINE DER_2D_CD2O1



!$$$$$$ !######################################################################
!$$$$$$ SUBROUTINE DERIVATIVE_ST_FD1O4(DER,A,DIR)
!$$$$$$     USE MOD_PARAM
!$$$$$$ 
!$$$$$$ SUBROUTINE DERIVATIVE_ST_FD1O4
!$$$$$$ 
!$$$$$$ 
!$$$$$$ !######################################################################
!$$$$$$ SUBROUTINE DERIVATIVE_ST_BD1O4(DER,A,DIR)
!$$$$$$     USE MOD_PARAM
!$$$$$$ 
!$$$$$$ SUBROUTINE DERIVATIVE_ST_BD1O4


!######################################################################
     
        
END MODULE MOD_DERIVATIVE