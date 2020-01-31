	include 'common.inc'

c	structure station_parameter save parameter and observation data at station
c	id:				id of the station
c	x,y,elev:		Lambert_x, Lambert_y, elevation of the station
c	Other parameter:See readme.txt with the data file (as below)
c    ƽ������  �������    �������  ƽ�����ʪ�� ��С���ʪ�� ������    ������    ƽ������  ������  ������      ��ˮ��  0���׵����¶�  ����ʱ��
c                                                 �����µ�λ��0.1���϶ȣ�        �����ʪ�ȵ�λ��%��     ��0.1�ɣ�       �����ٵ�λ��0.1��/�룩         ��0.1���ף���0.1���϶ȣ� ��0.1ʱ��    E_pan	Snow                    
c    tm      tmax      tmin        um      umin    n_summ    n_lowm       fsm     fmaxx     fmaxs      rsum       d0m       sun		E0		snow
	structure /station_parameter/
		integer	id
		real	x,y,elev
		integer	nodata(max_time)!if nodat.ne.0 then the station has data
		real	tm(max_time),tmax(max_time),tmin(max_time)
		real	um(max_time),umin(max_time),n_summ(max_time),n_lowm(max_time)
		real	fsm(max_time),fmaxx(max_time),fmaxs(max_time)
		real	rsum(max_time),d0m(max_time),sun(max_time)
		real	E0(max_time),snow(max_time)
	end structure
	record	/station_parameter/ stn(max_stn)
	
