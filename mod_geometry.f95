MODULE MOD_GEOMETRY
CONTAINS
!###########################################
	SUBROUTINE GEOMETRY_SRC_SURF_STLINE(BC,SFX,SLX,SDX,DEPTH,G,M)
    USE MOD_PARAM
    
    IMPLICIT NONE
    TYPE(BOUNDARY_PML),INTENT(IN)	:: BC
	REAL(KIND=DP), INTENT(IN)		:: SFX, SLX, SDX, DEPTH
    TYPE(GEOMETRY_SNR),INTENT(INOUT):: G
    TYPE(MODEL),INTENT(INOUT)		:: M
	
	INTEGER  :: I

	IF ((SLX>M%NX*M%DH).OR.(DEPTH>M%NZ*M%DH)) THEN
        PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        PRINT*, 'ERROR: IN SOURCE POSITION'
        PRINT*, 'DESCRIPTION: SOURCE LIES OUTSIDE THE MODEL'
    	WRITE(*,'(A30,F10.3,A3,F10.3)') 'MODEL SIZE:',M%NX*M%DH,'X',M%NZ*M%DH
		WRITE(*,'(A30,F10.3,A3,F10.3)') 'SOURCE LAST POSITION :', SLX,'X',DEPTH
        PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        PRINT*,''
        STOP
	END IF

    G%NSRC=FLOOR((SLX-SFX)/SDX) + 1
    !PRINT* , 'NSRC', G%NSRC
	ALLOCATE(G%SX(G%NSRC)) !,G%SZ(G%NSRC), G%SXN(G%NSRC),G%SZN(G%NSRC))
	ALLOCATE(G%SZ(G%NSRC))	
    ALLOCATE(G%SXN(G%NSRC))	
    ALLOCATE(G%SZN(G%NSRC))    
    !PRINT*,'FLAG :: ', DEPTH
    
    DO I=1,G%NSRC
        G%SX(I) = SFX + (I-1)*SDX
        G%SZ(I) = DEPTH
        G%SXN(I)= NINT(G%SX(I)/M%DH)
        G%SZN(I)= NINT(G%SZ(I)/M%DH)
        PRINT*, G%SX(I),G%SZ(I), G%SXN(I), G%SZN(I)
    END DO

    IF(BC%TOPBC=='TOPFS')THEN
	    G%SXN = G%SXN + BC%NAB
    ELSE IF(BC%TOPBC=='TOPABC')THEN
    	G%SZN = G%SXN + BC%NAB
        G%SXN = G%SXN + BC%NAB
    ELSE
		PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        PRINT*,'ERROR: BOUNDARY CONDITION '
        PRINT*,"DESCRIPTION: BC DOESN'T MATCH TO ANY INTRINSIC PRESCRIBED TYPE "
        PRINT*, 'PRESENT BC TYPE: ', BC%TOPBC
        PRINT*,'SEE MANUAL FOR HELP'
        PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        STOP
    END IF
	

    END SUBROUTINE GEOMETRY_SRC_SURF_STLINE
	
	!=================================================
    SUBROUTINE GEOMETRY_REC_SURF_STLINE(BC,RFX, RLX,RDX,DEPTH,G,M)
    USE MOD_PARAM
    IMPLICIT NONE
	TYPE(BOUNDARY_PML),INTENT(IN)	:: BC
	REAL(KIND=DP), INTENT(IN)		:: RFX, RLX, RDX, DEPTH
    TYPE(GEOMETRY_SNR),INTENT(INOUT):: G
    TYPE(MODEL),INTENT(INOUT)		:: M

	INTEGER  :: I

	IF ((RLX>M%NX*M%DH).OR.(DEPTH>M%NZ*M%DH)) THEN
        PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        PRINT*, 'ERROR: IN RECIEVERS POSITION'
        PRINT*, 'DESCRIPTION: RECIEVERS LIES OUTSIDE THE MODEL'
    	WRITE(*,'(A30,F10.3,A3,F10.3)') 'MODEL SIZE: ',M%NX*M%DH,'X',M%NZ*M%DH
		WRITE(*,'(A30,F10.3,A3,F10.3)') 'RECIEVERS LAST POSITION : ', RLX,'X',DEPTH
        PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        PRINT*,''
        STOP
	END IF
                
    G%NREC=FLOOR((RLX-RFX)/RDX) + 1
    !PRINT* , 'NSRC', G%NSRC
	ALLOCATE(G%RX(G%NREC)) !,G%SZ(G%NSRC), G%SXN(G%NSRC),G%SZN(G%NSRC))
	ALLOCATE(G%RZ(G%NREC))	
    ALLOCATE(G%RXN(G%NREC))	
    ALLOCATE(G%RZN(G%NREC))    
    !PRINT*,'FLAG :: ', DEPTH
    
    DO I=1,G%NREC
        G%RX(I) = RFX + (I-1)*RDX
        G%RZ(I) = DEPTH
        G%RXN(I)= FLOOR(G%RX(I)/M%DH)
        G%RZN(I)= FLOOR(G%RZ(I)/M%DH)
        PRINT*, G%RX(I),G%RZ(I), G%RXN(I), G%RZN(I)
    END DO

   IF(BC%TOPBC=='TOPFS')THEN
	    G%RXN = G%RXN + BC%NAB
    ELSE IF(BC%TOPBC=='TOPABC')THEN
    	G%RZN = G%RXN + BC%NAB
        G%RXN = G%RXN + BC%NAB
    ELSE
		PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        PRINT*,'ERROR: BOUNDARY CONDITION '
        PRINT*,"DESCRIPTION: BC DOESN'T MATCH TO ANY INTRINSIC PRESCRIBED TYPE "
        PRINT*, 'PRESENT BC TYPE: ', BC%TOPBC
        PRINT*,'SEE MANUAL FOR HELP'
        PRINT*,"!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!"
        STOP
    END IF
    
    END SUBROUTINE GEOMETRY_REC_SURF_STLINE
	
!###########################################
END MODULE MOD_GEOMETRY