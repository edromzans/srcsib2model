c	This subroutine read Goode projection ancillary files, get lat and lon array
c	Input:
c		FileLat,FileLon:	File name of ancillary files
c		ipix_max,iscan_max:	Size of array lat, lon
c		ipix,iscan:			Size of lat,lon files
c	Output:
c		reallat,reallon:	lat, lon array
c	Note:	use /assume:byterecl when compile the file
c			Byte swap, change data Big_endian <-> Little_endian
	subroutine	Read_ancillary_file(FileLat,FileLon,
     $	ipix_max,iscan_max,reallat,reallon,ipix,iscan)
	implicit none
	CHARACTER*80	FileLat,FileLon
	integer			ipix_max,iscan_max,nr,nc,I,ipix,iscan
	real			reallat(iscan_max,ipix_max)
	real			reallon(iscan_max,ipix_max)
c	CHARACTER*2		Clat,Clon,Ctmp
	integer*2		lat,lon !,lat_tmp,lon_tmp
	real			rlat,rlon
c	EQUIVALENCE		(Clat,lat_tmp)
c	EQUIVALENCE		(Clon,lon_tmp)
c	Read Asia map latitude and longitude files, get the lat and lon array
      open(10,form='unformatted',file=FileLat,access='direct',recl=2)
      open(20,form='unformatted',file=FileLon,access='direct',recl=2)
	nr=1
	nc=1
      Do I= 1,iscan*ipix
		read(10,rec=I) lat
		read(20,rec=I) lon
c		lat_tmp=lat				!Change to little_endian
c		Ctmp=Clat
c		Clat(1:1)=Ctmp(2:2)
c		Clat(2:2)=Ctmp(1:1)
c		lat=lat_tmp
c		lon_tmp=lon				!Change to little_endian
c		Ctmp=Clon
c		Clon(1:1)=Ctmp(2:2)
c		Clon(2:2)=Ctmp(1:1)
c		lon=lon_tmp
		if(lat.gt.9.and.lat.lt.18011) then	!Change unsign integer to real
			rlat=(lat-9010)*0.01
		else
			rlat=-9999
		endif
		if(lon.gt.9.and.lon.lt.36011) then	!Change unsign integer to real
			rlon=(lon-18010)*0.01
		else
			rlon=-9999
		endif
		reallat(nr,nc)=rlat
		reallon(nr,nc)=rlon
		nc=nc+1
		if (nc.gt.ipix) then
			nc=1
			nr=nr+1
		endif					
      End do
      close(20)
      close(10)
	end

	subroutine	Read_ancil_file_2(FileLat,FileLon,
     $	ipix_max,iscan_max,reallat,reallon,ipix,iscan,Offset,gain)
	implicit none
	CHARACTER*80	FileLat,FileLon
	integer			ipix_max,iscan_max,nr,nc,I,ipix,iscan
	real			reallat(iscan_max,ipix_max)
	real			reallon(iscan_max,ipix_max)
	real			Offset,gain
c	CHARACTER*2		Clat,Clon,Ctmp
	integer*2		lat,lon !,lat_tmp,lon_tmp
	real			rlat,rlon
c	EQUIVALENCE		(Clat,lat_tmp)
c	EQUIVALENCE		(Clon,lon_tmp)
c	Read Asia map latitude and longitude files, get the lat and lon array
      open(10,form='unformatted',file=FileLat,access='direct',recl=2)
      open(20,form='unformatted',file=FileLon,access='direct',recl=2)
	nr=1
	nc=1
      Do I= 1,iscan*ipix
		read(10,rec=I) lat
		read(20,rec=I) lon
c		lat_tmp=lat				!Change to little_endian
c		Ctmp=Clat
c		Clat(1:1)=Ctmp(2:2)
c		Clat(2:2)=Ctmp(1:1)
c		lat=lat_tmp
c		lon_tmp=lon				!Change to little_endian
c		Ctmp=Clon
c		Clon(1:1)=Ctmp(2:2)
c		Clon(2:2)=Ctmp(1:1)
c		lon=lon_tmp
		if(lat.gt.-9000.and.lat.lt.9000) then	!Change unsign integer to real
			rlat=(lat-Offset)*gain
		else
			rlat=-9999
		endif
		if(lon.gt.-18000.and.lon.lt.18000) then	!Change unsign integer to real
			rlon=(lon-Offset)*gain
		else
			rlon=-9999
		endif
		reallat(nr,nc)=rlat
		reallon(nr,nc)=rlon
		nc=nc+1
		if (nc.gt.ipix) then
			nc=1
			nr=nr+1
		endif					
      End do
      close(20)
      close(10)
	end