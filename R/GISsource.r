######		GIS_source.r　　　　　　　2008-2012　Ishida Megumi (Gifu univ.)
######	 主な内容
######	　GISに関する基礎関数
######	　緯度・経度→平面直角座標系変換
######	　カシミール3Dのトラックデータ（.trk）、ウェイポイントデータ（.wpt）の平面直角座標系変換





### 変数定義

degree=pi/180




#' calculate polygonal area
#'
#' @param xl
#' @param yl
#'
#' @return
#' @export
#'
#' @examples
area<-function(xl,yl){
	xl2<-c(xl[2:length(xl)],xl[1])
	yl2<-c(yl[2:length(yl)],yl[1])
      sum((xl2-xl)*(yl+yl2)/2)
}

#### 点間距離

#' disd
#'
#' @param xl
#' @param yl
#'
#' @return
#' @export
#'
#' @examples
distance<-function(xl,yl){
	xl2<-c(xl[2:length(xl)],xl[1])
	yl2<-c(yl[2:length(yl)],yl[1])
      sqrt((xl2-xl)^2+(yl2-yl)^2)
				}

### 最寄り点の抽出　list<-検索先ベクトル・データ　q<-問い合わせ・データ
#' Title
#'
#' @param list
#' @param q
#'
#' @return
#' @export
#'
#' @examples
nearest <-  function(list,q){which(abs(list-q)==min(abs(list-q)))}[1]

##　カシミールtrkファイルの行から緯度、経度データ取得　(ddd.dddddd 形式で保存されたもの)
#' Title
#'
#' @param s
#'
#' @return
#' @export
#'
#' @examples
lonlat <- function(s) {as.numeric(c(substr(s,17,27),substr(s,5,14)))}

##　カシミールwptファイルの行から緯度、経度データ取得　(ddd.dddddd 形式で保存されたもの)
#' Title
#'
#' @param s
#'
#' @return
#' @export
#'
#' @examples
lonlatWPT <- function(s) {as.numeric(c(substr(s,24,34),substr(s,12,21)))}

##　カシミールwptファイルの行からラベルデータ取得　
#' Title
#'
#' @param s
#'
#' @return
#' @export
#'
#' @examples
textWPT <- function(s) {sub(" .* ","",substr(s,61,100))}

##### trkll　カシミールのトラック行データから緯度・経度を抽出する関数
#' Title
#'
#' @param s
#'
#' @return
#' @export
#'
#' @examples
trkll<-function(s){
lon=as.numeric(substr(s,17,27));lat=as.numeric(substr(s,5,14))
c(lon,lat)
}

##### trkXY　カシミールのトラック行データからXYを抽出する関数
#' Title
#'
#' @param s
#'
#' @return
#' @export
#'
#' @examples
trkXY<-function(s){
lon=as.numeric(substr(s,17,27));lat=as.numeric(substr(s,5,14))
ll2xy(lat*degree,lon*degree) ###+GPSdxy
}

##### trkXYn　カシミールのn番目のトラックデータからXYを抽出する関数
#' Title
#'
#' @param dir
#' @param filename
#' @param ii
#'
#' @return
#' @export
#'
#' @examples
trkXYn<-function(dir,filename,ii){
	##dir=GISdir;filename="BeltTransect.trk"
 	l<-readLines(paste(dir,"/",filename,sep=""))
 	dn=length(l)
	q="H  LATITUDE    LONGITUDE    DATE      TIME     ALT"
	mm<-which(l==q)
	mmm<-append(mm,dn+4)

	s<-lonlat(l[(mmm[ii]+1):(mmm[ii+1]-4)]);
	n=length(s);
	lon=s[1:(n/2)];lat=s[(n/2+1):n];
	d<-ll2xy(lat*degree,lon*degree)
	(TRKx<-d[(n/2+1):n])
	(TRKy<-d[1:(n/2)])
	(d<-matrix(c(TRKx,TRKy),ncol=2))
				}
##### trkXYall　
#' カシミールの全トラックデータからリスト形式でXYを抽出する関数
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
trk<-function(filename){
	### filename="../gps/位山演習林林道.trk"

       l<-readLines(filename)
 	dn=length(l)
	q="H  LATITUDE    LONGITUDE    DATE      TIME     ALT"
	mm<-which(l==q)
	mmm<-append(mm,dn+4)

      z<-list()
      for(ii in 1:(length(mmm)-1)){
		s<-lonlat(l[(mmm[ii]+1):(mmm[ii+1]-4)]);
		n=length(s);
		lon=s[1:(n/2)];lat=s[(n/2+1):n];
		d<-ll2xy(lat*degree,lon*degree)
		TRKx<-d[(n/2+1):n]
		TRKy<-d[1:(n/2)]
		d<-matrix(c(TRKx,TRKy),ncol=2)
		lbl<-substr(l[mmm[ii]-2],3,100)
		z<-append(z,list(d))
		names(z)[ii]<-substr(l[mmm[ii]-2],3,100)
            	                 }
      return(z)
				}

##### wptXY　カシミールのウェイポイント・データからXY、ラベルを抽出する関数
#' Title
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
 wpt<-function(filename){
	##filename<-"../gps/演習林範囲2.wpt"
 	l<-readLines(filename)
	mm<-substring(l,1,1)=="W"
	s<-lonlatWPT(l[mm])
	txt<-textWPT(l[mm])
	n=length(s);
	lon=s[1:(n/2)];lat=s[(n/2+1):n];
	d<-ll2xy(lat*degree,lon*degree)
	return(data.frame(x=d[(n/2+1):n],y=d[1:(n/2)],name=txt))
		}



 wptXYtxt<-function(dir,filename){
	##dir=GISdir;filename="waypoints.wpt"
 	l<-readLines(paste(dir,"/",filename,sep=""))
	mm<-substring(l,1,1)=="W"
	s<-lonlatWPT(l[mm])
	txt<-textWPT(l[mm])
	n=length(s);
	lon=s[1:(n/2)];lat=s[(n/2+1):n];
	d<-ll2xy(lat*degree,lon*degree)
	(WPTx<-d[(n/2+1):n])
	(WPTy<-d[1:(n/2)])
	(d<-matrix(c(WPTx,WPTy),ncol=2))
       list(d,txt)
		}



####################################
####################################

#' Title
#'
#' @param JGD7dv
#' @param JGD7dh
#'
#' @return
#' @export
#'
#' @examples
XY7Code <-function(JGD7dv,JGD7dh){
(JGD7code<-paste(
LETTERS[11+floor(-JGD7dv/30000)],
LETTERS[5+floor(JGD7dh/40000)],
formatC(abs(floor((-JGD7dv%%30000)/1000)),width=2,flag="0"),
formatC(abs(floor((JGD7dh%%40000)/1000)),width=2,flag="0"),
sep=""))

dv1<--30000*(which(LETTERS==substring(JGD7code,1,1))-11)
dv2<--(1+1000*as.numeric(substring(JGD7code,3,4)))
(dv<-dv1+dv2)
dh1<-40000*(which(LETTERS==substring(JGD7code,2,2))-5)
dh2<-1+1000*as.numeric(substring(JGD7code,5,6))
(dh<-dh1+dh2)
(dn<-floor((JGD7dh-dh)/2)-floor((JGD7dv-dv)/2)*500+1)
list(JGD7code,dn,dv,dh)
}



#' Title
#'
#' @param JGD7dv
#' @param JGD7dh
#'
#' @return
#' @export
#'
#' @examples
AltXY7<-function(JGD7dv,JGD7dh){
fn<-XY7Code(JGD7dv,JGD7dh)[[1]]
dn<-XY7Code(JGD7dv,JGD7dh)[[2]]
fn<-dir(path=DEMdir,pattern =paste(fn,".*.txt",sep=""))
d<-read.table(paste(DEMdir,"/",fn,sep=""),sep=",")
d[dn,3]
}



######
###### 　国土地理院　平面直角座標変換 （第7系）
###### 　　http://www.gsi.go.jp/LAW/heimencho.html
######     http://vldb.gsi.go.jp/sokuchi/surveycalc/algorithm/
######     http://vldb.gsi.go.jp/sokuchi/surveycalc/bl2xyf.html
######     TKY2JGD
######　ll2xy   緯度経度（wgs84）     →　第7系平面直角座標
######　ll2xy   第7系平面直角座標変換 →　緯度経度（wgs84）



m0 = 0.9999;
a = 6378137;
f = 1/298.257222101;
F = 1/f;
e1 = sqrt(2*f - f^2);
e2 = sqrt((e1^2)/(1 - e1^2));


radian <- function(degree) { degree/180*pi }

######　角度表現　文字<->数値
#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
s2n<-function(d){
   i=nchar(unlist(strsplit(d,"\\."))[1])+1;
  as.numeric(substr(d,1,i-1))+as.numeric(substr(d,i+1,i+2))/60+
  (as.numeric(substr(d,i+3,i+4))+as.numeric(paste(".",substr(d,i+5,20),sep="")))/3600
   }


#' Title
#'
#' @param d
#'
#' @return
#' @export
#'
#' @examples
#' d=137.13416566
#' n2s(d)
#' s2n(n2s(d))
#' #########  第7系原点
#' lat0 = s2n("36.00000")*degree;
#' lon0 = s2n("137.100000")*degree;

n2s<-function(d)
{s1 = floor(d);
    d2 = 60*(d - floor(d));
    s2 = substring(1000 + floor(d2), 3, 4);
    d3=60*(d2-floor(d2));
    s3 = substring(10000 + 10*d3, 3, 5);
       paste(s1,".", s2 ,s3,sep="")}






#' Title
#'
#' @param lat
#' @param lat00
#' @param lon00
#'
#' @return
#' @export
#'
#' @examples
S<-function(lat,lat00="36.00000",lon00="137.100000"){
　##### lat00・lon00 座標系原点の緯度・経度　【デフォルトは第7系】
　#####　http://www.gsi.go.jp/LAW/heimencho.html
　#####　http://vldb.gsi.go.jp/sokuchi/surveycalc/bl2xyf.html
　　lat0 = s2n(lat00)*degree;　lon0 = s2n(lon00)*degree;

    ########### 5.緯度を与えて赤道からの子午線弧長を求める計算###############

    f = 1/298.257222101;
    e1 = sqrt(2*f - f^2);
    cA = 1 + 3/4*e1^2 + 45/64*e1^4 + 175/256*e1^6 + 11025/16384*e1^8 +
        43659/65536*e1^10 + 693693/1048576*e1^12 + 19324305/29360128*e1^14 +
        4927697775/7516192768*e1^16;
    cB = 3/4*e1^2 + 15/16*e1^4 + 525/512*e1^6 + 2205/2048*e1^8 +
        72765/65536*e1^10 + 297297/262144*e1^12 + 135270135/117440512*e1^14 +
        547521975/469762048*e1^16;
    cC = 15/64*e1^4 + 105/256*e1^6 + 2205/4096*e1^8 + 10395/16384*e1^10 +
        1486485/2097152*e1^12 + 45090045/58720256*e1^14 +
        766530765/939524096*e1^16;
    cD = 35/512*e1^6 + 315/2048*e1^8 + 31185/131072*e1^10 +
        165165/524288*e1^12 + 45090045/117440512*e1^14 +
        209053845/469762048*e1^16;
    cE = 315/16384*e1^8 + 3465/65536*e1^10 + 99099/1048576*e1^12 +
        4099095/29360128*e1^14 + 348423075/1879048192*e1^16;
    cF = 693/131072*e1^10 + 9009/524288*e1^12 + 4099095/117440512*e1^14 +
        26801775/469762048*e1^16;
    cG = 3003/2097152*e1^12 + 315315/58720256*e1^14 +
        11486475/939524096*e1^16;
    cH = 45045/117440512*e1^14 + 765765/469762048*e1^16;
    cI = 765765/7516192768*e1^16;
    B1 = a*(1 - e1^2)*cA;
    B2 = a*(1 - e1^2)*(-cB/2);
    B3 = a*(1 - e1^2)*(cC/4);
    B4 = a*(1 - e1^2)*(-cD/6);
    B5 = a*(1 - e1^2)*(cE/8);
    B6 = a*(1 - e1^2)*(-cF/10);
    B7 = a*(1 - e1^2)*(cG/12);
    B8 = a*(1 - e1^2)*(-cH/14);
    B9 = a*(1 - e1^2)*(cI/16);
    B1*lat + B2*sin(2*lat) + B3*sin(4*lat) + B4*sin(6*lat) + B5*sin(8*lat) +
      B6*sin(10*lat) + B7*sin(12*lat) + B8*sin(14*lat) + B9*sin(16*lat)

 }



#' Title
#'
#' @param x
#' @param y
#' @param lat00
#' @param lon00
#'
#' @return
#' @export
#'
#' @examples
xy2ll<-function(x,y,lat00="36.00000",lon00="137.100000"){
　##### lat00・lon00 座標系原点の緯度・経度　【デフォルトは第7系】
　#####　http://www.gsi.go.jp/LAW/heimencho.html
　　lat0 = s2n(lat00)*degree;　lon0 = s2n(lon00)*degree;

    M = S(lat0) + y/m0;
    lat1 =
      lat0 + (2*(S(lat0) - M)*(1 - e1^2*sin(lat0)^2)^(3/2))/(3*
                e1^2*(S(lat0) - M)*sin(lat0)*
                cos(lat0)*(1 - e1^2*sin(lat0)^2)^(1/2) - 2*a*(1 - e1^2));
    N1 = a/(sqrt(1 - e1^2*sin(lat1)^2));
      eta12 = e2^2*cos(lat1)^2;
      t1 = tan(lat1);
      ##### 1 - 1 緯度 ######
      lat = lat1 - 1/2*1/(N1^2)*t1*(1 + eta12)*(x/m0)^2+1/24*1/N1^4*
            t1*(5 + 3*t1^2 + 6*t1^2 - 6*t1^2*eta12 - 3*eta12^2
            - 9*t1^2*eta12^2)*(x/m0)^4-1/720*1/N1^6*t1*(61 + 90*t1^2
            + 45*t1^2 + 107*eta12 - 162*t1^2*eta12)*(x/m0)^6+
            1/40320*1/N1^8*t1*(1385 + 3633*t1^2 + 4095*t1^4 +
            1575*t1^6)*(x/m0)^8;
      ###### 1 - 2 経度 #######
     dlon = 1/(N1*cos(lat1))*(x/m0)
        - 1/6*1/(N1^3*cos(lat1))*(1 + 2*t1^2 + eta12)*(x/m0)^3+
        1/120*1/(N1^5*cos(lat1))*(5 + 28*t1^2
        +24*t1^4*6*eta12*8*t1^2*eta12)*(x/m0)^5-
        1/5040*1/(N1^7*cos(lat1))*(61 + 662*t1^2 +
        1320*t1^4 + 720*t1^6)*(x/m0)^7;

     dlon = 1/(N1*cos(lat1))*(x/m0)- 1/6*1/(N1^3*cos(lat1))*
        (1 + 2*t1^2 + eta12)*(x/m0)^3+1/120*1/(N1^5*cos(lat1))*
        (5 + 28*t1^2+24*t1^4*6*eta12*8*t1^2*eta12)*(x/m0)^5-
        1/5040*1/(N1^7*cos(lat1))*(61 + 662*t1^2 +1320*t1^4
        + 720*t1^6)*(x/m0)^7;


    lon = lon0 + dlon;
    c(lat, lon)/degree

}




#' Title
#'
#' @param lat
#' @param lon
#' @param lat00
#' @param lon00
#'
#' @return
#' @export
#'
#' @examples
ll2xy<-function (lat,lon,lat00="36.00000",lon00="137.100000"){
　##### lat00・lon00 座標系原点の緯度・経度　【デフォルトは第7系】
　#####　http://www.gsi.go.jp/LAW/heimencho.html
　　lat0 = s2n(lat00)*degree;　lon0 = s2n(lon00)*degree;

    dlon = lon - lon0;
    eta2 = e2*cos(lon)^2 ;
    t = tan(lat);
    N0 = a/sqrt(1 - e1^2*sin(lat)^2);
    ### 2 - (1)x座標 ####

    x = ((S(lat) - S(lat0)) + 1/2*N0*cos(lat)^2*t*dlon^2 +
            1/24*N0*cos(lat)^4*t*(5 - t^2 + 9*eta2 + 4*eta2^2)*dlon^4
            -
            1/720*N0*cos(lat)^6*
              t*(-61 + 58*t^2 - t^4 - 270*eta2 + 330*t^2*eta2)*dlon^6
            -
            1/40320*N0*cos(lat)^8*t*(-1385 + 3111*t^2 - 543*t^t + t^6)*
              dlon^8)*m0;
    #### 2 - (2)y座標 ####
    y = (N0*cos(lat)*dlon- 1/6*N0*cos(lat)^3*(-1 + t^2 - eta2)*dlon^3
            - 1/120*N0*cos(lat)^5*(-5 + 18*t^2 - t^4 - 14*eta2 +
            58*t^2*eta2)*dlon^5- 1/5040*N0*cos(lat)^7*(-61 + 479*t^2
            - 179*t^4 + t^6)*dlon^7)*m0;
   c(x, y)
			}

###  R image ->   Grass ASCII raster grid


#' Title
#'
#' @param we
#' @param sn
#' @param m
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
R2Grass_AsciiRaster<-function(we,sn,m,filename){
	## we<-x;sn<-y;m<-dem2m
	rows=length(sn);cols=length(we)
	dsn<-(sn[length(sn)]-sn[1])/(rows-1)
	dwe<-(we[length(we)]-we[1])/(cols-1)
	south=sn[1]-dsn/2;north=sn[length(sn)]+dsn/2
	west=we[1]-dwe/2;east=we[length(we)]+dwe/2


	h_<-paste("north: ",north,"\n","south: ",south,"\n","east: ",east,"\n","west: ",west,
 		"\n","rows: ",rows,"\n","cols: ",cols,"\n",sep="")

	m2<-m[,ncol(m):1]
	m2[is.na(m2)]<-"*"

	cat(h_,file=filename)
	for(i in 1:ncol(m2)) cat(m2[,i],"\n",file=filename,append=T)

            						}

######





### Grass ASCII raster grid -> R image

#' Title
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
Grass2R_AsciiRaster<-function(filename){

	d0<-readLines(filename)    ### ex. filename<-"ascii-grid-dem.txt"

	### header
	h0<-d0[1:6]; h0<-unlist(strsplit(h0,": "))
	h<-as.numeric(h0[seq(2,12,2)]);names(h)<-h0[seq(1,11,2)]

	### dem data
	d<-d0[7:length(d0)]
	m<-matrix(as.numeric(unlist(strsplit(d," "))),ncol=h["cols"],byrow=T)
	m<-t(m)[,ncol(t(m)):1]

	dsn<-(h["north"]-h["south"])/h["rows"]
	dwe<-(h["east"]-h["west"])/h["cols"]
	sn<-seq(h["south"]+dsn/2,h["north"],dsn)
	we<-seq(h["west"]+dwe/2,h["east"],dwe)
	data<-list(west_east=we,south_north=sn,matrix=m)
	return(data)
							}


#######################################################
###　塗りつぶし関数
#######################################################

#' Title
#'
#' @param xnn
#' @param ynn
#' @param m
#' @param cl
#' @param cl00
#'
#' @return
#' @export
#'
#' @examples
fill_HL<-function(xnn,ynn,m,cl,cl00){
	ynn1<-range(ynn)[1]; ynn2<-range(ynn)[2];
	x12y<-c()

	for (i in ynn1:ynn2) {
		j<-sort(xnn[ynn==i]);k<-which(diff(j)>1);
		if(length(k)==1){x12y<-rbind(x12y,c(j[k],j[k+1],i))}
					}

	for (i in 1:nrow(x12y))	m[x12y[i,1]:x12y[i,2],x12y[i,3]]<-cl

				return(m)
						}

fill_VL<-function(xnn,ynn,m,cl,cl00){
	xnn1<-range(xnn)[1]; xnn2<-range(xnn)[2];
	xy12<-c()

	for (i in xnn1:xnn2) {
		j<-sort(ynn[xnn==i]);k<-which(diff(j)>1);
		if(length(k)==1){xy12<-rbind(xy12,c(i,j[k],j[k+1]))}
					}

	for (i in 1:nrow(xy12))	m[xy12[i,1],xy12[i,2]:xy12[i,3]]<-cl

				return(m)
						}

fill_L<-function(xnn,ynn,m,cl,cl00){
		m<-fill_HL(xnn,ynn,m,cl,cl00)
		m<-fill_VL(xnn,ynn,m,cl,cl00)
					return(m)
						}

fill2<-function(xnn, ynn,m,cl,cl00)
{	########　塗りつぶしプログラム　ベクターポリゴン→ラスター
	######## 　fill_L　(line method) によって塗り残した部分を　fill 再帰関数によって塗りつぶし
	######## 　ライン法と周囲探索法(再帰関数)による　スタック不足対策
	########　　ポリゴンデータ　行　xnn 　列　ynn 　　マトリクスデータ　m　塗りつぶす色　cl　塗りつぶされる色　cl00

	fill<-function(x,y,cl,cl00) {
		 ## cl00<-0;cl<-1    			　　 ####　cl00 塗りつぶされる値
   		 m[x,y]<<-cl; 　　　　 ####　塗りつぶす値
		if (m[x,y-1]==cl00) fill(x,y-1,cl,cl00)
		if (m[x+1,y]==cl00) fill(x+1,y,cl,cl00)
		if (m[x,y+1]==cl00) fill(x,y+1,cl,cl00)
		if (m[x-1,y]==cl00) fill(x-1,y,cl,cl00)
				}

	m<-fill_L(xnn,ynn,m,999,cl00)			######　　ライン法で塗りつぶし　999で仮塗り

	m[cbind(xnn,ynn)]<-cl				#####　境界セルの再描画　xnn　ynn
	ij<-which(m==999,arr.ind =TRUE)		####　セル検出
	rn<-ij[,1];	cn<-ij[,2]

								######## 対象セルの周辺カラー
	c4<-cbind(m[cbind(rn,cn-1)],m[cbind(rn+1,cn)],m[cbind(rn,cn+1)],m[cbind(rn-1,cn)])
	rcc4<-data.frame(rn,cn,y_1=c4[,1],x1=c4[,2],y1=c4[,3],x_1=c4[,4])

								####　　塗りつぶし損ねのセル err_cell の検出
	ij2<-which(c4==0,arr.ind =TRUE)
	i<-ij2[,1];rn2<-rn[i];cn2<-cn[i]; ## rcc4[i,]
	j<-ij2[,2]; drn<-j; dcn<-j
	drn[j==1]<-0 ;drn[j==2]<- 1;drn[j==3]<- 0;drn[j==4]<--1
	dcn[j==1]<--1;dcn[j==2]<- 0;dcn[j==3]<- 1;dcn[j==4]<- 0
	err_cell<-cbind(rn2+drn,cn2+dcn)

	while(nrow(err_cell)!=0){
		fill(err_cell[1,1],err_cell[1,2],999,0)	###  再帰関数による塗りつぶし
		err_cell<-err_cell[m[err_cell]==0,]		###　塗りつぶし損ねのセル err_cell の再構築
					}

	m[m==999]<-cl						### 999　->　cl の書き戻し
	return(m)
}



fill_<-function(x,y,m,cl,cl00){
	## 未完成　袋小路で停止してしまう　
	## x<-p1x_n;y<-p1y_n
    m[x,y]<-cl; 					####　cl00 塗りつぶされる値  cl　塗りつぶす値
    repeat{
	ifelse(m[x,y-1]==cl00,{m[x,y-1]<-cl;y<-y-1},
	 ifelse(m[x+1,y]==cl00,{m[x+1,y]<-cl;x<-x+1},
	   ifelse(m[x,y+1]==cl00,{m[x,y+1]<-cl;y<-y+1},
	     ifelse(m[x-1,y]==cl00,{m[x-1,y]<-cl;x<-x-1},
			break))))
		}
			return(m)
					}

#######################################################
#######################################################

#' Title
#'
#' @param px00
#' @param py00
#' @param p1x
#' @param p1y
#' @param xaxis
#' @param yaxis
#' @param m
#' @param cl
#' @param cl00
#'
#' @return
#' @export
#'
#' @examples
Polygon2Raster<-function(px00,py00,p1x,p1y,xaxis,yaxis,m,cl,cl00){

                        ###px00,py00,p1x,p1y,xaxis,yaxis,m,cl,cl00
	##############################################
	## ポリゴン内のメッシュの取得　再帰的方法
	## px00,py00 ポリゴンデータ　（ベクトル）
	## px1, py1 ポリゴン内部点　
	## xaxis,yaxis,m　イメージ・マトリクス　DEM等
	##　cl00 塗りつぶされる値
	##　塗りつぶす値
	##############################################


	px<-px00;py<-py00

						### 近似点置換

	for (i in 1:length(px)){
			px[i]<-xaxis[nearest(xaxis,px00[i])]
			py[i]<-yaxis[nearest(yaxis,py00[i])]
				}


	n<-length(px)										### 点データ間の補間
	xxx<-c();yyy<-c()

	for (i in 1:(n-1)){
		if(px[i]<px[i+1])rng<-(xaxis[2]-xaxis[1]) else rng<--(xaxis[2]-xaxis[1])
		xx<-seq(px[i],px[i+1],rng)
		dpx<-px[i+1]-px[i]
		if (dpx!=0)yy<-py[i]+(py[i+1]-py[i])/dpx*(xx-px[i])
		else{yy<-py[i]}
		xxx<-c(xxx,xx);yyy<-c(yyy,yy)
	}


	for (i in 1:(n-1)){
		if(py[i]<py[i+1])rng<-(yaxis[2]-yaxis[1]) else rng<--(yaxis[2]-yaxis[1])
		yy<-seq(py[i],py[i+1],rng)
		dpy<-py[i+1]-py[i]
		if (dpy!=0)xx<-px[i]+(px[i+1]-px[i])/dpy*(yy-py[i])
		else{xx<-px[i]}
		xxx<-c(xxx,xx);yyy<-c(yyy,yy)
				}


											###　境界線→ラスター

xnn<-c();for (i in 1:length(xxx))xnn<-c(xnn,nearest(xaxis,xxx[i]))
ynn<-c();for (i in 1:length(yyy))ynn<-c(ynn,nearest(yaxis,yyy[i]))
m[cbind(xnn,ynn)]<-cl

#####################　ポリゴン内→ラスター
　	p1x_n<-nearest(xaxis,p1x);p1y_n<-nearest(yaxis,p1y)
	m<-fill2(xnn,ynn,m,cl,cl00)
##################


	return(m)
}



#' Title
#'
#' @param PolygonsList
#' @param Points
#' @param xaxis
#' @param yaxis
#'
#' @return
#' @export
#'
#' @examples
PolygonsList2Raster<-function(PolygonsList,Points,xaxis,yaxis){
	pl<-PolygonsList
	p<-Points
	m<<-matrix(0,nrow=length(xaxis),ncol=length(yaxis))

	for (i in 1:length(pl)){
	px<-pl[[i]][,1];py<-pl[[i]][,2]
	p1x<-p[i,1];p1y<-p[i,2]

		m<<-Polygon2Raster(px,py,p1x,p1y,xaxis,yaxis,m,i,0)
					}

            return(m)

					}

######################
###### 座標の回転
#' Title
#'
#' @param XX
#' @param YY
#' @param x00
#' @param y00
#' @param deg
#'
#' @return
#' @export
#'
#' @examples
RotateXY<-function(XX,YY,x00,y00,deg){
## XX<-c(0,2,1);YY<-c(0,0,2);x00<-0;y00<-0;deg<-45
	dx=XX-x00; dy=YY-y00; dl=sqrt(dx^2+dy^2);
	deg2=atan2(YY-y00,XX-x00)+deg/180*pi;
	 matrix(c(x00+dl*cos(deg2),y00+dl*sin(deg2)),ncol=2)
		}
######################

### GIMPで作成したパスファイルを.svgでエクスポートし座標データを抽出する
#' Title
#'
#' @param f
#'
#' @return
#' @export
#'
#' @examples
svgXY<-function(f){
	l<-readLines(f)
	ll<-strsplit(l,split=c(" "));ll<-unlist(ll)
	ll<-strsplit(ll,",");ll<-unlist(ll)
	ll<-strsplit(ll,"\"");ll<-unlist(ll)

	i<-which(ll=="viewBox=");
	rng<-as.numeric(ll[i+1:4]);
	x1<-rng[1];y1<-rng[2];x2<-rng[3];y2<-rng[4]

	c<-which(ll=="C")+1;z<-which(ll=="Z")-1
	lll<-c()
	for(ii in 1:length(c)){
	lll<-c(lll,
	list(matrix(as.numeric(ll[c[ii]:z[ii]]),ncol=2,byrow="T"))
			)
				}
	return(list(range=data.frame(x=c(x1,x2),y=c(y1,y2)),xy=lll))
####### example #######################
if(0){
dat<-svgXY("test2.svg")
rng<-dat$range
xy<-dat$xy
names(dat)
str(dat)
plot(1,type="n",xlim=rng$x,ylim=rng$y)
for(ii in 1:length(xy))polygon(xy[[ii]])
for(ii in 1:length(lll))text(mean(xy[[ii]][,1]),mean(xy[[ii]][,2]),ii,cex=0.7,col="red")
}
#######################################
			}

#' Title
#'
#' @param px00
#' @param py00
#' @param xaxis
#' @param yaxis
#'
#' @return
#' @export
#'
#' @examples
hokan<-function(px00,py00,xaxis,yaxis){

	##############################################
	## px,py ラインデータ　（ベクトル）
	## x,y　対応座標
	##############################################


	px<-px00;py<-py00

						### 近似点置換

	for (i in 1:length(px)){
			px[i]<-xaxis[nearest(xaxis,px00[i])]
			py[i]<-yaxis[nearest(yaxis,py00[i])]
				}


	n<-length(px)			### 点データ間の補間
	xxx<-c();yyy<-c()

	for (i in 1:(n-1)){
		if(px[i]<px[i+1])rng<-(xaxis[2]-xaxis[1]) else rng<--(xaxis[2]-xaxis[1])
		xx<-seq(px[i],px[i+1],rng)
		dpx<-px[i+1]-px[i]
		if (dpx!=0)yy<-py[i]+(py[i+1]-py[i])/dpx*(xx-px[i])
		else{yy<-py[i]}
		xxx<-c(xxx,xx);yyy<-c(yyy,yy)
				}


	for (i in 1:(n-1)){
		if(py[i]<py[i+1])rng<-(yaxis[2]-yaxis[1]) else rng<--(yaxis[2]-yaxis[1])
		yy<-seq(py[i],py[i+1],rng)
		dpy<-py[i+1]-py[i]
		if (dpy!=0)xx<-px[i]+(px[i+1]-px[i])/dpy*(yy-py[i])
		else{xx<-px[i]}
		xxx<-c(xxx,xx);yyy<-c(yyy,yy)
				}

　　　　　　　　　return(cbind(xxx,yyy))
			}



#' Title
#'
#' @param xaxis
#' @param yaxis
#' @param px00
#' @param py00
#'
#' @return
#' @export
#'
#' @examples
l2r<-function(xaxis,yaxis,px00,py00){

	##############################################
	## px,py ラインデータ　（ベクトル）
	## x,y　イメージ・座標
	##############################################
	px<-px00;py<-py00
	xy_<-hokan(px00,py00,xaxis,yaxis)
	xxx<-xy_[,1]; yyy<-xy_[,2];


											###　境界線→ラスター
	xn<-length(xaxis);yn<-length(yaxis)	####　0マトリクス作成
	m<-matrix(0,nrow=xn,ncol=yn)
	xnn<-c();for (i in 1:length(xxx))xnn<-c(xnn,nearest(xaxis,xxx[i]))
	ynn<-c();for (i in 1:length(yyy))ynn<-c(ynn,nearest(yaxis,yyy[i]))
	m[cbind(xnn,ynn)]<-1
	return(m)
										}

#' Title
#'
#' @param xaxis
#' @param yaxis
#' @param pl
#'
#' @return
#' @export
#'
#' @examples
ll2r<-function(xaxis,yaxis,pl){

        ##############################################
        ## pl ラインデータ・リスト　（px,py ベクトル）
        ## x,y　イメージ・座標
        ##############################################
	 ### xaxis<-x;yaxis<-y;pl<-rind
        xn<-length(xaxis);yn<-length(yaxis)     ####　0マトリクス作成
        m<-matrix(0,nrow=xn,ncol=yn)


	for (ii in 1:length(pl)){			###　境界線→ラスター
        px<-pl[[ii]][,1];py<-pl[[ii]][,2]
        xy_<-hokan(px,py,xaxis,yaxis)
        xxx<-xy_[,1]; yyy<-xy_[,2];
        xnn<-c();for (i in 1:length(xxx))xnn<-c(xnn,nearest(xaxis,xxx[i]))
        ynn<-c();for (i in 1:length(yyy))ynn<-c(ynn,nearest(yaxis,yyy[i]))
        m[cbind(xnn,ynn)]<-1
    				   }
        		return(m)
                                }



#' Title
#'
#' @param v0
#'
#' @return
#' @export
#'
#' @examples
split0<-function(v0){
	##v0<-c(1,2,3,0,4,5,6,0,0,7,8)
	i<-which(v0!=0)					#### 対象点抽出
	j<-which(head(i,-1)!=tail(i,-1)-1)+1		####　連続部分抽出
	k<-c(1,j,length(i)+1)				####　区切り範囲
	nn<-length(k)-1					####　ブロック数
							####　リスト化
	v_<-c(); for (ii in 1:nn)v_<-c(v_,list(i[k[ii]:(k[ii+1]-1)]))
	return(v_)
				}

#### saitan
#' Title
#'
#' @param xx0
#' @param yy0
#' @param xx
#' @param yy
#'
#' @return
#' @export
#'
#' @examples
saitan<-function(xx0,yy0,xx,yy){
	saitan0<-function(xx0,yy0)min(sqrt((xx0-xx)^2+(yy0-yy)^2))
	mapply(saitan0,xx0,yy0)
				}

#' Title
#'
#' @param XY
#'
#' @return
#' @export
#'
#' @examples
TotalLength<-function(XY){
  sum(distance(XY[,1],XY[,2]))
}

TotalLength2<-function(l,pn){
	#####　リストに分割された道を結合　
	##l<-rind; 						##トラック・リスト
	##pn<-lapply(lapply(rind,XYnen),split0)	##人工林内のポイントの番号
	ln<-length(l)
	TL<-c();for (ii in 1:length(l)){
	tl<-c();for (i in 1:length(pn[[ii]])){
		j<-pn[[ii]][[i]]
		##if (is.na(j))break
		if(length(j)>1)tl<-c(tl,TotalLength(l[[ii]][j,]))
						}
		TL<-c(TL,list(tl))
					}
		return(TL)
					}


######
#' Title
#'
#' @param X
#' @param Y
#' @param x1
#' @param y1
#' @param x2
#' @param y2
#' @param x1_
#' @param y1_
#' @param x2_
#' @param y2_
#'
#' @return
#' @export
#'
#' @examples
affineXY <- function(X,Y,x1,y1,x2,y2,x1_,y1_,x2_,y2_){
	#X=c(1:5,1:5);Y=rep(c(1,5),each=5) 	# oroginal coordinates
	#x1=1;y1=1;x2=5;y2=5				# oroginal coordinates of 2 points
	#x1_=30;y1_=30;x2_=25;y2_=40		# conversion coordinates of  2 points (xy1, xy2)
	dx <- x2-x1   ; dy <- y2-y1   ;dx_<- x2_-x1_ ; dy_<- y2_-y1_
	dxx<- x1_-x1  ; dyy<- y1_-y1
	da <-atan2(dy_,dx_)-atan2(dy,dx)
	X <-X+dxx ; Y <- Y+dyy
	XY <- RotateXY(X,Y,x1_,y1_,da*180/pi)
	return(XY)
}



###########################################
#' Title
#'
#' @param filename
#' @param id
#'
#' @return
#' @export
#'
#' @examples
shplines <- function(filename="./shp/2021池塘line-4",id="ID_1"){
	d <- shapefiles::read.shapefile(filename)
	dbf <- d$dbf$dbf	# names(dbf)
	ID<-as.vector(d$dbf$dbf[,id])
	l <- c()
	for(i in 1:nrow(dbf)){
  	 l <- c(l,list(data.frame(d$shp$shp[[i]]$points,Z=0)))
  	}
	names(l) <- ID
	return(l)
}

#' Title
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
shppoints <- function(filename="./shp/2021池塘line-4-points"){
	d <- shapefiles::read.shapefile(filename)
	dbf<-d$dbf$dbf
	xy<-d$shp$shp[,c(2,3)]
	return(data.frame(xy,dbf))
}


