c	abstract study area zone from global or asia file
c	Input:
c		Dname:		Filename of global or asia data file
c		D_nc,D_nr:	cols and rows of global or asia data
c		S_c,S_r:	start col and row of the study zone
c		S_nc,S_nr:cols and rows of study area	
c		ID:	ID=1, file is unsign integer 1 byte, otherwise 2 bytes data
c	Output:
c		Save in files
c		KER =1, OK = others, Error
	subroutine Abstract_SA(Dname,D_nc,D_nr,S_c,S_r,S_nc,S_nr,ID,KER)
	CHARACTER*80	Dname,Oname
	integer			D_nc,D_nr,S_c,S_r,S_nc,S_nr
	CHARACTER*1		buf1(D_nc)
	CHARACTER*2		buf2(D_nc)
	KER=0
	Call Strlen(Dname,n1,n2)
	Oname=Dname(n1:n2)//'_SA'
	if (ID.eq.1) then	
		open(1,form='unformatted',file=Dname,access='direct',
     $		recl=D_nc,status='old',err=4000)
		open(2,form='unformatted',file=Oname,access='direct',recl=S_nc)
	else
		open(1,form='unformatted',file=Dname,access='direct',
     $		recl=D_nc*2,status='old',err=4000)
		open(2,form='unformatted',file=Oname,access='direct',recl=S_nc*2)
	endif
	do i = S_r , S_r+S_nr-1
		if (ID.eq.1) then
			read (1,rec=i,err=4001) buf1
			write(2,rec=i-S_r+1,err=4002) (buf1(j),j=S_c,S_c+S_nc-1)
		else
			read (1,rec=i,err=4001) buf2
			write(2,rec=i-S_r+1,err=4002) (buf2(j),j=S_c,S_c+S_nc-1)
		endif
	enddo
	close(2)
	close(1)
	RETURN
4000	KER=1
	PRINT*,'OPEN FILE ERROR:',Dname(n1:n2)
	RETURN
4001	KER=2
	PRINT*,'READ FILE ERROR:',Dname(n1:n2)
	PRINT*,'Please try to compile as: f77 xxx.f -assume:byterecl'
	RETURN
4002	KER=3
	PRINT*,'WRITE FILE ERROR:',Dname(n1:n2)//'_SA'
	RETURN
	end