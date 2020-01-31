c	This subroutine patch up Tiled files to one file
c	Input:
c		PatchDir:	The directory where Tiled files are in
c		Prefix:		The prefix of Tiled file name
c		Suffix:		The suffix of Tiled file name
c		Col0,Col1:	Start/End Col number
c		Row0,Row1:	Start/End Row number
c		NT:			Times of the saved data, e.g. if one month data are save in one file
c						then it will be 29/30/31 time daily data, so NT=29/30/31
c		ID:			ID=1, file is unsign integer 1 byte, otherwise 2 bytes data
c	Output:
c		OUTPUT file will be in the same directory as input files
      subroutine Patch_Tiled_file(PatchDir,Prefix,Suffix,
     $	Col0,Col1,Row0,Row1,NT,ID,KER)
	implicit none	
	CHARACTER*80	PatchDir,Prefix,Suffix,Fname,Fout
	CHARACTER*2		CharR,CharC,CharR0,CharR1,CharC0,CharC1
	integer			Col0,Col1,Row0,Row1,ID,KER
	CHARACTER*125	buf1
	CHARACTER*250	buf2
	integer			i,j,k,m,ii,jj,l1,l2,ll1,ll2,s1,s2,rec2,NT
	KER=0
	Call Strlen(PatchDir,l1,l2)
	Call Strlen(Prefix,ll1,ll2)
	Call Strlen(Suffix,s1,s2)
	call ConvI2C(Row0,CharR0)
	call ConvI2C(Row1,CharR1)
	call ConvI2C(Col0,CharC0)
	call ConvI2C(Col1,CharC1)
	Fout=PatchDir(l1:l2)//Prefix(ll1:ll2)//
     $	CharC0//CharR0//CharC1//CharR1//Suffix(s1:s2)
	if (ID.eq.1) then
		open(2,form='unformatted',file=Fout,access='direct',recl=125)
	else 
		open(2,form='unformatted',file=Fout,access='direct',recl=250)
	endif

	do i=Row0,Row1
	do j=Col0,Col1
		call ConvI2C(i,CharR)
		call ConvI2C(j,CharC)
		Fname=PatchDir(l1:l2)//Prefix(ll1:ll2)//
     $		CharC//CharR//Suffix(s1:s2)
		if (ID.eq.1) then
			open(1,form='unformatted',file=Fname,
     $			access='direct',recl=125,status='old',err=9100)
		else 
			open(1,form='unformatted',file=Fname,
     $			access='direct',recl=250,status='old',err=9100)
		endif
		ii=i-Row0+1
		jj=j-Col0+1
		do m=1,NT
			do k=1,125	
				if (ID.eq.1) then	
					read (1,rec=(m-1)*125+k,err=9101) buf1	
				else
					read (1,rec=(m-1)*125+k,err=9101) buf2	
				endif			
				rec2=(m-1)*(Col1-Col0+1)*(Row1-Row0+1)*125
     $				+(Col1-Col0+1)*((ii-1)*125+(k-1))+jj
				if (ID.eq.1) then	
					write(2,rec=rec2,err=9101) buf1
				else
					write(2,rec=rec2,err=9101) buf2
				endif			
			enddo
		enddo
		close(1)		
	enddo
	enddo
	close(2)
	RETURN
9100	KER=1
	Call Strlen(Fname,l1,l2)
	PRINT *, 'WARNING: CANNOT OPEN FILE:',Fname(l1:l2)
	RETURN
9101	KER=2
	Call Strlen(Fname,l1,l2)
	PRINT *, 'ERROR OCCUR WHEN READ FILE:',Fname(l1:l2)
	RETURN
9102	KER=3
	Call Strlen(Fout,l1,l2)
	PRINT *, 'ERROR OCCUR WHEN WRITE FILE:',Fout(l1:l2)
	RETURN
	end

