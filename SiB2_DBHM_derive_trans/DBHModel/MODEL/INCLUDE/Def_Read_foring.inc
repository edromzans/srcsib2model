	include 'common.inc'

c	structure station_parameter save parameter and observation data at station
c	id:				id of the station
c	x,y,elev:		Lambert_x, Lambert_y, elevation of the station
c	Other parameter:See readme.txt with the data file (as below)
c    平均气温  最高气温    最低气温  平均相对湿度 最小相对湿度 总云量    低云量    平均风速  最大风向  最大风速      降水量  0厘米地面温度  日照时数
c                                                 （气温单位：0.1摄氏度）        （相对湿度单位：%）     （0.1成）       （风速单位：0.1米/秒）         （0.1毫米）（0.1摄氏度） （0.1时）    E_pan	Snow                    
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
	

