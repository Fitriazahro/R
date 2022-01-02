#Halaman Awal

library(tcltk)
library(tcltk2)
jendela<-tktoplevel()
tktitle(jendela)<-"UJI KRUSKAL WALLIS"

frame1<-tkframe(jendela,relief="groove",borderwidth=20,bg="aquamarine")
frame2<-tkframe(jendela,relief="solid",borderwidth=7,bg="aqua")
frame3<-tkframe(jendela,relief="groove",borderwidth=15,bg="aquamarine")
frame4<-tkframe(jendela,relief="solid",borderwidth=7,bg="aqua")
frame5<-tkframe(jendela,relief="groove",borderwidth=15,bg="aquamarine")
frame6<-tkframe(jendela,relief="solid",borderwidth=7,bg="aqua")
frame7<-tkframe(jendela,relief="groove",borderwidth=15,bg="aquamarine")
frame8<-tkframe(jendela,relief="flat",borderwidth=20,bg="aqua")
frame9<-tkframe(jendela,relief="groove",borderwidth=5,bg="black")

text1=tkfont.create(family="sans",size=18,weight="bold")
text2=tkfont.create(family="times",size=14,weight="bold")
text3=tkfont.create(family="times",size=12,slant="italic")
text4=tkfont.create(family="courier",size=12)
text5=tkfont.create(family="times",size=12,weight="bold")

tkpack(tklabel(frame1,text="K-WIDG",font=text1,bg="aquamarine"))
tkpack(frame1,fill="both")
tkpack(tklabel(frame2,text="Kruskal-Wallis Widget",font=text3,bg="aqua"))
tkpack(frame2,fill="both")
tkpack(tklabel(frame3,text="KELOMPOK 4 KELAS S3",font=text2,bg="aquamarine"))
tkpack(frame3,fill="both")
tkpack(tklabel(frame4,text="Mega Putri Gusti Safitri\t    (081911833003)",font=text4,bg="aqua"))
tkpack(tklabel(frame4,text="Nurul Nur Azizah\t\t    (081911833011)", font=text4,bg="aqua"))
tkpack(tklabel(frame4,text="Fitria Halimatuzzahro'\t            (081911833019)",font=text4,bg="aqua"))
tkpack(tklabel(frame4,text="Cindyana Supriadi\t\t    (081911833036)", font=text4,bg="aqua"))
tkpack(frame4,fill="both")
tkpack(tklabel(frame5,text="DOSEN PEMBIMBING",font=text2,bg="aquamarine"))
tkpack(frame5,fill="both")
tkpack(tklabel(frame6,text="Dr. Toha Saifudin, M.Si.",font=text4,bg="aqua"))
tkpack(tklabel(frame6,text="Dr. Nur Chamidah, M.Si.",font=text4,bg="aqua"))
tkpack(frame6,fill="both")
tkpack(tklabel(frame7,text="Tekan Tombol MULAI untuk masuk ke halaman utama",font=text3,bg="aquamarine"))
tkpack(frame7,fill="both")

#Halaman Utama

menuutama<-function()
{
  tkdestroy(jendela)
  library(tcltk)
  library(tcltk2)
  jendela<-tktoplevel()
  tktitle(jendela)<-"Halaman Utama"
  
  frame11<-tkframe(jendela,relief="groove",borderwidth=5)
  frame12<-tkframe(jendela,relief="groove",borderwidth=5,bg="aqua")
  frame13<-tkframe(jendela,relief="groove",borderwidth=5,bg="aquamarine")
  tekst1<-tkfont.create(family="times",size=15,weight="bold")
  tekst2<-tkfont.create(family="times",size=12,weight="bold")
  tekst3<-tkfont.create(family="sans",size=12)
  tekst4<-tkfont.create(family="sans",size=13,slant="italic")
  frame10<-tkframe(jendela,relief="solid",borderwidth=5,bg="aquamarine")
  #Kata Sambutan 
  tkpack(tklabel(frame10,text="SELAMAT DATANG DI HALAMAN UTAMA K-WIDG",font=tekst1,bg="aquamarine"))
  tkpack(frame10,fill="both")
  tkpack(tklabel(frame12,text="K-Widg adalah suatu widget sederhana",font=tekst4,bg="white"))
  tkpack(tklabel(frame12,text="untuk melakukan uji Kruskal-Wallis secara mudah",font=tekst4,bg="white"))
  tkpack(frame12,fill="both")
  tkpack(tklabel(frame13,text=" ",bg="aquamarine"))
  tkpack(tklabel(frame13,text="Untuk menggunakan uji Kruskal-Wallis, silakan menuju menu 'Uji Kruskal Wallis'",font=tekst3,bg="aquamarine"))
  tkpack(tklabel(frame13,text="Jika Anda memerlukan penjelasan terkait Uji Kruskal-Wallis, silakan menuju menu 'Teori' dan 'Bantuan'",font=tekst3,bg="aquamarine"))
  tkpack(tklabel(frame13,text=" ",bg="aquamarine"))
  tkpack(frame13,fill="both")
  #menu
  topmenu<-tkmenu(jendela)
  submenu<-tkmenu(jendela)
  tkconfigure(jendela,menu=topmenu)
  menu1<-tkmenu(topmenu,tearoff=F)
  sm1<-tkmenu(submenu,tearoff=F)
  tkadd(menu1,"command",label="Uji Normalitas",command=function()normal())
  tkadd(menu1,"command",label="Impor Data",command=function()impor())
  tkadd(menu1,"command",label="Ringkasan Data",command=function()B())
  menu2<-tkmenu(topmenu,tearoff=F)
  menu3<-tkmenu(topmenu,tearoff=F)
  tkadd(menu3,"command",label="Penjelasan",command=function()help())
  tkadd(menu3,"command",label="Contoh Soal", command=function()cs1())
  tkadd(topmenu,"cascade",label="Uji Kruskal Wallis",menu=menu1)
  tkadd(topmenu,"command",label="Teori",command=function()A1())
  tkadd(topmenu,"cascade",label="Bantuan",menu=menu3)
  #menutup program
  tutupmenu<-function()
  {
    tkdestroy(jendela)
    tkmessageBox(message="Terima Kasih")
  }
  tutup<-tkbutton(frame11,text="SELESAI",bg="aqua",font=tekst2,command=tutupmenu)
  tkpack(tutup,side="bottom")
  tkpack(frame11,side="bottom")
}

#tombol menuju ke menu utama

home<-tkbutton(frame9,text="MULAI",bg="aqua",font=text5,command=menuutama)
tkpack(home,side="bottom")
tkpack(frame9,side="bottom")

#fungsi menu teori
    
 
#-----------------------------UJI NORMALITAS-------------------------------#
normal<-function()
{
  require(tcltk)
  d1<-tktoplevel(bg="pink")
  tktitle(d1)<-"Import Data"
  t3<-tkfont.create(family="pink",size=16)
  tkgrid(tklabel(d1,text=" ",bg="pink"))
  tkgrid(tklabel(d1,text="IMPOR DATA",font=t3,bg="white"),sticky="n")
  tkgrid(tklabel(d1,text="UJI NORMALITAS",font=t3,bg="white"),sticky="n")
  h0<-tclVar("Data Berdistribusi Normal ")
  h1<-tclVar("Data Tidak Berdistribusi Normal ")
  alpha<-tclVar(" ")
  tkgrid(tklabel(d1,text="",bg="pink"))
  eb7<-tkentry(d1,width="25",textvariable=alpha)
  tkgrid(tklabel(d1,text="Alpha",font="t3",bg="white"),sticky="n")
  tkgrid(eb7)
  tkgrid(tklabel(d1,text="",bg="pink"))
  
  
  normal1<-function()
  { 
    library(foreign)
    library(tcltk)
    library(readxl)
    data1<-read_excel(file.choose(),sheet=1,col_names=TRUE)
    
    #~~~~~~~~~~ CHECK TABEL ~~~~~~~~~~~~~#
    cek<-function()
    {
      require(tcltk)
      cek1<-tktoplevel(bg="white")
      tktitle(cek1)<-"Check Data"
      tclRequire("Tktable")
      
      x<-as.matrix(data1)
      n1<-nrow(data1)
      kol<-ncol(data1)
      v<-View(x)
      
      isitabel1<-tclArray()
      for (i in 0:n1)
      {
        for (j in 0:kol)
        {
          isitabel1[[i,j]]=x[i,j]
        }
      }
      
      tabel<-tkwidget(cek1,"table",variable=isitabel1,rows=(n1+1),cols=(kol+1),titlerows=1,selectmode="extended  ",colwidth=10,background="white")
      tkconfigure(tabel,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
      
      tkgrid(tabel,sticky="n")
      
      tomboltutup<-tkbutton(cek1,text="TUTUP",command=function()tkdestroy(cek1))
      tkgrid(tomboltutup,sticky="e")
    }
    
    ######################
    analisis<-function()
    {
      library(foreign)
      require(tcltk)
      require(foreign)
      library(mnormt)
      library(mvnormtest)
      library(readxl)
      h00<-as.character(tclvalue(h0))
      h11<-as.character(tclvalue(h1))
      alpha1<-as.numeric(tclvalue(alpha))
      m=as.matrix(data1)
      aaa=shapiro.test(m)
      c=as.numeric(round(aaa[[2]],2))
      d=as.numeric(round(aaa[[1]],3))
      
      if(alpha1==" ")
      {
        tkmessageBox(message="Anda belum memasukkan alpha",icon="warning")
      }
      
      else
      {
        j2<-tktoplevel(bg="aquamarine")
        tktitle(j2)<-"Hasil Uji Normalitas"
        scr<-tkscrollbar(j2,orient="vertical",command=function(...)tkyview(teks1,...))
        teks1<-tktext(j2,bg="pink",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
        tkgrid(teks1,scr,sticky="news")
        tkgrid.rowconfigure(j2,teks1,weight=1)
        tkgrid.columnconfigure(j2,teks1,weight=1)
        
        tkinsert(teks1,"end",paste("
		=========================================
		        HASIL UJI NORMALITAS
		=========================================
	
		HIPOTESIS\t:
		~~
		H0\t: ",h00,"
		H1\t: ",h11,"

		DAERAH KRITIS\t:
		~~~
		Tolak Ho Jika
		Signifikansi < ",(alpha1),"
		
		STATISTIK UJI\t:
		~~
     \t\tSignifikansi = ", c,"
    \t\tStatistik Uji W = ", d))
        
        
        if (c > alpha1)
        {
          tkinsert(teks1,"end",paste("
		
		KEPUTUSAN\t:
		Terima Ho
		Karena\t",c,">",(alpha1)," 

		KESIMPULAN\t:",h00,"
                                  
    \t        Data ini lebih cocok diuji menggunakan One Way ANOVA"))
        }
        
        else 
        {
          tkinsert(teks1,"end",paste("
		
		KEPUTUSAN\t:
		Tolak Ho
		Karena\t",c,"<",(alpha1)," 


		KESIMPULAN\t:", h11,"
    
    \t        Silakan lanjutkan Uji Kruskal Wallis!"))
        }
        tombol<-tkbutton(j2,text="back",command=function()tkdestroy(j2))
        tkgrid(tombol)
        tkconfigure(teks1,state="disabled")
      }
    }
    
    tkdestroy(back)
    
    tombolcek<-tkbutton(d1,text="LIHAT DATA",bg="darkseagreen",command=cek)
    tkgrid(tombolcek,padx=5,pady=5)
    
    tombolanalisis<-tkbutton(d1,text="ANALISIS",bg="darkseagreen",command=analisis)
    tkgrid(tombolanalisis,padx=5,pady=5)
    
    
    kembali=tkbutton(d1,text="KEMBALI KE MENU",bg="darkseagreen",command=function()tkdestroy(d1))
    tkgrid(kembali,sticky="n",padx=5,pady=5)
    
    
  }
  
  tkgrid(tklabel(d1,text=" ",bg="pink"))
  impor2<-tkbutton(d1,text="IMPORT",bg="darkseagreen",command=normal1)
  tkgrid(impor2,sticky="n",padx=10,pady=10)
  
  
  back<-tkbutton(d1,text="KEMBALI KE MENU",bg="darkseagreen",command=function()tkdestroy(d1))
  tkgrid(back,sticky="n",padx=10,pady=10)
  
  tkgrid(tklabel(d1,text=" ",bg="pink"))
  tkgrid(tklabel(d1,text="*CATATAN : Import data hanya dapat menggunakan format .xls (exel)",bg="white"))
  tkgrid(tklabel(d1,text="*Inputkan alpha dalam bentuk desimal (misal : 0.05)",bg="white"))
  tkgrid(tklabel(d1,text=" ",bg="pink"))
  
}




 




#--------------------------------- IMPOR DATA -----------------------------------#
impor<-function()
{
  require(tcltk)
  dataimpor<-tktoplevel(bg="pink")
  tktitle(dataimpor)<-"Import Data"
  t3<-tkfont.create(family="pink",size=20)
  tkgrid(tklabel(dataimpor,text=" ",bg="pink"))
  tkgrid(tklabel(dataimpor,text="IMPOR DATA",font=t3,bg="white"),sticky="n")
  tkgrid(tklabel(dataimpor,text="UJI KRUSKAL WALLIS",font=t3,bg="white"),sticky="n")
  h0<-tclVar(" ")
  h1<-tclVar(" ")
  alpha<-tclVar(" ")
  tkgrid(tklabel(dataimpor,text="",bg="pink"))
  eb1<-tkentry(dataimpor,width="50",textvariable=h0)
  tkgrid(tklabel(dataimpor,text="Populasi",font="t3",bg="white"),sticky="n")
  tkgrid(eb1)
  eb2<-tkentry(dataimpor,width="50",textvariable=h1)
  tkgrid(tklabel(dataimpor,text=" ",bg="pink"))
  tkgrid(tklabel(dataimpor,text="Variabel Respon",font="t3",bg="white"),sticky="n")
  tkgrid(eb2)
  tkgrid(tklabel(dataimpor,text="",bg="pink"))
  eb7<-tkentry(dataimpor,width="50",textvariable=alpha)
  tkgrid(tklabel(dataimpor,text="Alpha",font="t3",bg="white"),sticky="n")
  tkgrid(eb7)
  tkgrid(tklabel(dataimpor,text="",bg="pink"))
  
  
  impor1<-function()
  { 
    library(foreign)
    library(tcltk)
    library(readxl)
    data1<-read_excel(file.choose(),sheet=1,col_names=TRUE)
    
    #~~~~~~~~~~ CHECK TABEL ~~~~~~~~~~~~~#
    cek<-function()
    {
      require(tcltk)
      cek1<-tktoplevel(bg="white")
      tktitle(cek1)<-"Check Data"
      tclRequire("Tktable")
      
      x<-as.matrix(data1)
      n1<-nrow(data1)
      kol<-ncol(data1)
      v<-View(x)
      
      isitabel1<-tclArray()
      for (i in 0:n1)
      {
        for (j in 0:kol)
        {
          isitabel1[[i,j]]=x[i,j]
        }
      }
      
      tabel<-tkwidget(cek1,"table",variable=isitabel1,rows=(n1+1),cols=(kol+1),titlerows=1,selectmode="extended  ",colwidth=10,background="white")
      tkconfigure(tabel,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
      
      tkgrid(tabel,sticky="n")
      
      tomboltutup<-tkbutton(cek1,text="TUTUP",command=function()tkdestroy(cek1))
      tkgrid(tomboltutup,sticky="e")
    }
    ####ANALISIS#######
    
    analisis<-function()
    {
      library(foreign)
      require(tcltk)
      require(foreign)
      library(readxl)
      h00<-as.character(tclvalue(h0))
      h11<-as.character(tclvalue(h1))
      alpha1<-as.numeric(tclvalue(alpha))
      
      aaa=kruskal.test(data1)
      c=as.numeric(round(aaa[[3]],5))
      d=as.numeric(round(aaa[[1]],4))
      m=as.matrix(data1)
      k=ncol(m)
      ttabel=round(qchisq(1-alpha1,k-1),4)
      
      if(h00==" "||alpha1==" ")
      {
        tkmessageBox(message="Anda belum memasukkan data",icon="warning")
      }
      
      else
      {
        dataimpor<-tktoplevel(bg="aquamarine")
        tktitle(dataimpor)<-"Hasil Uji Kruskall Wallis"
        scr<-tkscrollbar(dataimpor,orient="vertical",command=function(...)tkyview(teks1,...))
        teks1<-tktext(dataimpor,bg="pink",width=120,height=20,yscrollcommand=function(...)tkset(scr,...))
        tkgrid(teks1,scr,sticky="news")
        tkgrid.rowconfigure(dataimpor,teks1,weight=1)
        tkgrid.columnconfigure(dataimpor,teks1,weight=1)
        
        tkinsert(teks1,"end",paste("
		=========================================
		        HASIL UJI KRUSKAL WALLIS 
		=========================================
	
		HIPOTESIS\t:
		~~
		H0\t: Tidak ada perbedaan",h11,"pada",h00,"
		H1\t: Terdapat perbedaan",h11," pada",h00,"

		DAERAH KRITIS\t:
		~~~
		Tolak Ho Jika
		Signifikansi < ",(alpha1),"
		Statistik Uji H > ",(ttabel),"
		
		STATISTIK UJI\t:
		~~
		Statistik Uji H = ",d,"
     \t\tSignifikansi = ", c))
        
        
        if (c < alpha1)
        {
          tkinsert(teks1,"end",paste("
		
		KEPUTUSAN\t:
		Tolak Ho
		Karena\t",c,"<",(alpha1)," dan ",d,">",ttabel," 

		KESIMPULAN\t: Terdapat perbedaan",h11,"pada",h00))
        }
        
        else 
        {
          tkinsert(teks1,"end",paste("
		
		KEPUTUSAN\t:
		Terima Ho
		Karena\t",c,">",(alpha1)," dan ",d,"<",ttabel," 

		KESIMPULAN\t: Tidak terdapat perbedaan",h11,"pada", h00))
        }
        
        tombol<-tkbutton(dataimpor,text="back",command=function()tkdestroy(dataimpor))
        tkgrid(tombol)
        tkconfigure(teks1,state="disabled")
      }
      
      
    }
    tkdestroy(back)
    
    tombolcek<-tkbutton(dataimpor,text="LIHAT DATA",bg="darkseagreen",command=cek)
    tkgrid(tombolcek,padx=5,pady=5)
    
    tombolanalisis<-tkbutton(dataimpor,text="ANALISIS",bg="darkseagreen",command=analisis)
    tkgrid(tombolanalisis,padx=5,pady=5)
    
    
    kembali=tkbutton(dataimpor,text="KEMBALI KE MENU",bg="darkseagreen",command=function()tkdestroy(dataimpor))
    tkgrid(kembali,sticky="n",padx=5,pady=5)
  }
  
  tkgrid(tklabel(dataimpor,text=" ",bg="pink"))
  impor2<-tkbutton(dataimpor,text="IMPORT",bg="darkseagreen",command=impor1)
  tkgrid(impor2,sticky="n",padx=10,pady=10)
  
  
  back<-tkbutton(dataimpor,text="KEMBALI KE MENU",bg="darkseagreen",command=function()tkdestroy(dataimpor))
  tkgrid(back,sticky="n",padx=10,pady=10)
  
  tkgrid(tklabel(dataimpor,text=" ",bg="pink"))
  tkgrid(tklabel(dataimpor,text="*CATATAN : Import data hanya dapat menggunakan format .xls (exel)",bg="white"))
  tkgrid(tklabel(dataimpor,text="*Inputkan alpha dalam bentuk desimal (misal : 0.05)",bg="white"))
  tkgrid(tklabel(dataimpor,text=" ",bg="pink"))
  
}


#FUNGSI SUB MENU RINGKASAN DATA
B<-function()
{
  require(tcltk)
  j2<-tktoplevel(bg="AQUAMARINE")
  t1<-tkfont.create(family="cambria",size=10)
  t2<-tkfont.create(family="sans",size=12,weight="bold")
  t3<-tkfont.create(family="cambria",size=10,weight="bold")
  tkgrid(tklabel(j2,text="UJI KRUSKAL WALLIS MELALUI RINGKASAN DATA",font=t2,bg="AQUAMARINE"))
  tkgrid(tklabel(j2,text="dgn jumlah sampel (n) dari k populasi sama ",font=t2,bg="white"))
  require(tcltk)
  tktitle(j2)<-"Uji Kruskal Wallis"
  h0<-tclVar(" ")
  h1<-tclVar(" ")
  ko<-tclVar(" ")
  No<-tclVar(" ")
  no<-tclVar(" ")
  Rj<-tclVar(" ")
  t0<-tclVar(" ")
  alpha<-tclVar(" ")
  tkgrid(tklabel(j2,text="",bg="aquamarine"))
  eb1<-tkentry(j2,width="60",textvariable=h0)
  tkgrid(tklabel(j2,text="Populasi",font="t",bg="aquamarine"))
  tkgrid(eb1)
  eb2<-tkentry(j2,width="60",textvariable=h1)
  tkgrid(tklabel(j2,text="Variabel Respon",font="t",bg="aquamarine"))
  tkgrid(eb2)
  eb3<-tkentry(j2,width="60",textvariable=ko)
  tkgrid(tklabel(j2,text="Jumlah Kelompok Populasi (K)",font="t1",bg="aquamarine"))
  tkgrid(eb3)
  eb4<-tkentry(j2,width="60",textvariable=No)
  tkgrid(tklabel(j2,text="    N    ",font="t1",bg="aquamarine"))
  tkgrid(eb4)
  eb5<-tkentry(j2,width="60",textvariable=no)
  tkgrid(tklabel(j2,text="    n    ",font="t1",bg="aquamarine"))
  tkgrid(eb5)
  eb6<-tkentry(j2,width="60",textvariable=Rj)
  tkgrid(tklabel(j2,text="Sigma Kuadrat Rj",font="t1",bg="aquamarine"))
  tkgrid(eb6)
  eb8<-tkentry(j2,width="60",textvariable=t0)
  tkgrid(tklabel(j2,text="Sigma T",font="t1",bg="aquamarine"))
  tkgrid(eb8)
  eb7<-tkentry(j2,width="60",textvariable=alpha)
  tkgrid(tklabel(j2,text="Alpha",font="t1",bg="aquamarine"))
  tkgrid(eb7)
  tkgrid(tklabel(j2,text="",bg="aquamarine"))
  
  
  ######################
  analisis2<-function()
  {
    require(tcltk)
    
    h00<-as.character(tclvalue(h0))
    h11<-as.character(tclvalue(h1))
    koo<-as.numeric(tclvalue(ko))
    Noo<-as.numeric(tclvalue(No))
    noo<-as.numeric(tclvalue(no))
    Rj1<-as.numeric(tclvalue(Rj))
    t00<-as.numeric(tclvalue(t0))
    alpha1<-as.numeric(tclvalue(alpha))
    
    if(Rj1==0) hhit1<-((12/(Noo*(Noo+1)))*Rj1/noo)-(3*(Noo+1))
    else hhit1<-((12/(Noo*(Noo+1)))*(Rj1/noo)-(3*(Noo+1)))/(1-t00/((Noo*Noo*Noo)-Noo))
    ttabel=qchisq(1-alpha1,koo-1)
    
    if(h00==" "||koo==" "||noo==" "||Noo==" "||Rj1==" "||alpha1==" " )
    {
      tkmessageBox(message="Anda belum memasukkan data",icon="warning")
    }
    
    else
    {
      j2<-tktoplevel(bg="aquamarine")
      tktitle(j2)<-"Hasil Uji Kruskall Wallis"
      scr<-tkscrollbar(j2,orient="vertical",command=function(...)tkyview(teks1,...))
      teks1<-tktext(j2,bg="pink",width=100,height=20,yscrollcommand=function(...)tkset(scr,...))
      tkgrid(teks1,scr,sticky="news")
      tkgrid.rowconfigure(j2,teks1,weight=1)
      tkgrid.columnconfigure(j2,teks1,weight=1)
      
      tkinsert(teks1,"end",paste("
	=========================================
	        HASIL UJI KRUSKAL WALLIS 
	=========================================
	HIPOTESIS\t:
	~~~~
	H0\t: Tidak terdapat perbedaan",h11,"pada",h00,"
	H1\t: Terdapat perbedaan",h11,"pada",h00,"
		
		
	DAERAH KRITIS\t:
	~~~~~
	Tolak Ho Jika
	Statistik Uji H > Chi-Kuadrat (alpha, K-1) 
	Statistik Uji H > ",(ttabel),"		   

		
	STATISTIK UJI\t:
	~~~~~~
	Statistik Uji H = ",hhit1))
      
      
      if (hhit1 > ttabel)
      {
        tkinsert(teks1,"end",paste("
		
	KEPUTUSAN\t:
	~~~~
	Tolak Ho
	Karena\t",hhit1,">",(ttabel),"

	KESIMPULAN\t: Terdapat perbedaan",h11,"pada",h00))
      }
      
      else 
      {
        tkinsert(teks1,"end",paste("
		
	KEPUTUSAN\t:
	~~~~
	Terima Ho
	Karena\t",hhit1,"<",(ttabel),"


	KESIMPULAN\t: Tidak terdapat perbedaan",h11,"pada", h00))
        
      }
      tombol<-tkbutton(j2,text="back",command=function()tkdestroy(j2))
      tkgrid(tombol)
      tkconfigure(teks1,state="disabled")
    }
  }
  tkgrid(tklabel(j2,text=" CATATAN : ",bg="white"))
  tkgrid(tklabel(j2,text="*Jika Sigma kuadrat Rj berbentuk pecahan, inputkan dalam bentuk desimal (misal : 4.6)",bg="white"))
  tkgrid(tklabel(j2,text="*Jika tidak ada nilai yang sama, inputkan 0 pada Sigma T",bg="white"))
  tkgrid(tklabel(j2,text="*Inputkan alpha dalam bentuk desimal (misal : 0.05)",bg="white"))
  tombolok3<-tkbutton(j2,text="ANALISIS DATA",font="t1", bg="pink", command=analisis2)
  tkgrid(tombolok3)
  tombolok4<-tkbutton(j2,text="MENU UTAMA",font="teks1", bg="pink", command=function()tkdestroy(j2))
  tkgrid(tombolok4)
  tkgrid(tklabel(j2,text=" ",bg="aquamarine"))
}

#FUNGSI SUB MENU KETENTUAN UJI
A1<-function()
{
  require(tcltk)
  j1<-tktoplevel(bg="white")
  tktitle(j1)<-"Ketentuan Uji"
  t1<-tkfont.create(family="cambria",size=10)
  t2<-tkfont.create(family="sans",size=12,weight="bold")
  t3<-tkfont.create(family="cambria",size=10,weight="bold")
  tkgrid(tklabel(j1,text=" ",bg="white"))
  tkgrid(tklabel(j1,text="KETENTUAN UJI KRUSKAL WALLIS",font=t2,bg="white"))
  tkgrid(tklabel(j1,text=" ",bg="white"))
  tkgrid(tklabel(j1,text="  1. HIPOTESIS  ",font=t3,bg="aquamarine"),sticky="w")
  tkgrid(tklabel(j1,text="     Hipotesis pada Uji Kruskal Wallis sebagai berikut :  :",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="     Ho : tidak ada perbedaan median dari k populasi  ",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="     H1 : ada perbedaan median dari k populasi ",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="     Note : ",font=t1,bg="pink"),sticky="w")
  tkgrid(tklabel(j1,text="     Banyaknya Populasi (k) lebih dari 2 Populasi ",font=t1,bg="pink"),sticky="w")
  tkgrid(tklabel(j1,text=" ",bg="white"))
  tkgrid(tklabel(j1,text="  2. STATISTIK UJI ",font=t3,bg="aquamarine"),sticky="w")
  tkgrid(tklabel(j1,text="     Rumus yang digunakan dalam Uji Kruskal-Wallis, sebagai berikut : ",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="     a.)Tanpa Faktor Koreksi ",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="     H = [12/(N(N+1)]*[Sigma (i=1 sampai k)*((Ri^2)/ni)]-[3(N+1)]",font=t1,bg="pink"),sticky="w")
  tkgrid(tklabel(j1,text="     b.)Dengan Faktor Koreksi ",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="     H = ([12/(N(N+1)]*[Sigma (i=1 sampai k)*((Ri^2)/ni)]-[3(N+1)])/(1-sigma(T)/(N^3-N))",font=t1,bg="pink"),sticky="w")
  tkgrid(tklabel(j1,text="     dengan :",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="      k adalah banyak populasi",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="      ni adalah  banyak unit pada populasi ke-i",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="      N  adalah  banyak keseluruhan unit",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="      Ri adalah  jumlah peringkat dalam populasi ke-i",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="      sigma(T)=sigma(t^3)-sigma(t), t adalah banyak sampel yang nilainya sama",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text=" ",bg="white"))
  tkgrid(tklabel(j1,text="  3. KAIDAH KEPUTUSAN ",font=t3,bg="aquamarine"),sticky="w")
  tkgrid(tklabel(j1,text="     Pengambilan Keputusan Pada Uji Kruskal Wallis berdasarkan kriteria berikut  : .",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="     a. Ho diterima, jika Statistik Uji H < = (kurang dari sama dengan) Chi-Square dengan derajat bebas (K-1) dan alpha 5%  ",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text="     b. Ho ditolak jika Statistik Uji H > (lebih dari) Chi-Square dengan derajat bebas (K-1) dan alpha 5% ",font=t1,bg="white"),sticky="w")
  tkgrid(tklabel(j1,text=" ",bg="white"))
  tkgrid(tklabel(j1,text="  Sumber : Buku Statistika Non Prametrik oleh Dr.Suliyanto",font=t3,bg="pink"),sticky="w")
  tkgrid(tklabel(j1,text=" ",bg="white"))
  tt<-tkbutton(j1,text="TUTUP",bg="aquamarine",font=t2,command=function()tkdestroy(j1))
  tkgrid(tt)
}



############################### MENU 3 SUB MENU 1 ################################

#------------------------- PILIHAN TEORI dan LANGKAH ----------------------------#
help<-function()
{
  require(tcltk)
  bantu<-tktoplevel(bg="lightsteelblue")
  tktitle(bantu)<-"Help"
  t1<-tkfont.create(family="times",size=14)
  tkgrid(tklabel(bantu,text=" ",bg="lightsteelblue"))
  tkgrid(tklabel(bantu,text="  Silahkan pilih yang ingin Anda ketahui!  ",font=t1,bg="lightsteelblue"),sticky="w")
  tkgrid(tklabel(bantu,text=" ",bg="lightsteelblue"))
  pil1<-tkbutton(bantu,text="Pengertian dan Kegunaan",command=teori)
  tkgrid(pil1)
  tkgrid(tklabel(bantu,text=" ",bg="lightsteelblue"))
  
  pil3<-tkbutton(bantu,text="Langkah Kerja Program",command=langkah2)
  tkgrid(pil3)
  tkgrid(tklabel(bantu,text=" ",bg="lightsteelblue"))
  tkgrid(tklabel(bantu,text=" ",bg="lightsteelblue"))
  tt1<-tkbutton(bantu,text="Back To Menu",command=function()tkdestroy(bantu))
  tkgrid(tt1)
}

#----------------------------------- TEORI --------------------------------------#
teori<-function()
{
  require(tcltk)
  teori1<-tktoplevel(bg="darkseagreen")
  tktitle(teori1)<-"Pengertian dan Kegunaan"
  t1<-tkfont.create(family="times",size=14)
  t2<-tkfont.create(family="stencil",size=20)
  tkgrid(tklabel(teori1,text=" ",bg="darkseagreen"))
  tkgrid(tklabel(teori1,text=" PENGERTIAN DAN KEGUNAAN ",font=t2,bg="palegoldenrod"))
  tkgrid(tklabel(teori1,text=" UJI KRUSKAL WALLIS ",font=t2,bg="tan"))
  tkgrid(tklabel(teori1,text=" ",bg="darkseagreen"))
  tkgrid(tklabel(teori1,text=" Uji kruskal wallis adalah salah satu uji statistik non parametrik ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" berbasis peringkat yang tujuannya untuk menentukan adakah perbedaan ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" signifikan secara statistik antara lebih dari dua populasi yang diwakili k ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" sampel independen pada variabel dependen yang berskala data numerik ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" (interval atau rasio) dan skala ordinal. Uji ini merupakan alternatif uji One ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" Way ANOVA apabila data tidak memenuhi asumsi, seperti normalitas. ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" (Sujarweni, 2015) ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" ",bg="darkseagreen"))
  tkgrid(tklabel(teori1,text=" Untuk menentukan apakah k sampel independen berasal dari populasi ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" yang berbeda, Uji Kruskal Wallis menitikberatkan pada ranking dari data ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" yang digunakan Uji Kruskal Wallis menguji hipotesis nol bahwa k sampel ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" berasal dari populasi yang sama atau populasi identik dalam hal rata-ratanya. ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" Uji ini membuat anggapan bahwa variabel yang diamati mempunyai ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" distribusi kontinu dengan skala minimalnya adalah skala ordinal ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" (Siegel, 1997). ",font=t1,bg="lightseagreen"))
  tkgrid(tklabel(teori1,text=" ",bg="darkseagreen"))
  tt2<-tkbutton(teori1,text="Back To Help",command=function()tkdestroy(teori1))
  tkgrid(tt2)
  tkgrid(tklabel(teori1,text=" ",bg="darkseagreen"))
}


#--------------------- LANGKAH ANALISIS DENGAN IMPOR DATA -----------------------#
langkah2<-function()
{
  require(tcltk)
  step2<-tktoplevel(bg="paleturquoise")
  tktitle(step2)<-"Langkah Kerja Program"
  scr2<-tkscrollbar(step2,orient="vertical",command=function(...)tkyview(teks2,...))
  teks2<-tktext(step2,bg="palegreen",width=80,height=12,yscrollcommand=function(...)tkset(scr2,...))
  tkgrid(teks2,scr2,sticky="news")
  tkgrid.rowconfigure(step2,teks2,weight=1)
  tkgrid.columnconfigure(step2,teks2,weight=1)
  tkinsert(teks2,"end",paste("
 ------ LANGKAH-LANGKAH PENGUJIAN KRUSKAL WALLIS DENGAN IMPOR DATA ------
 
 1. Beralih ke Menu Utama
 
 2. Pilih Menu 1 (Uji Kruskal Wallis)
 
 3. Pilih Sub-Menu Impor Data
 
 4. Inputkan seluruh informasi yang diperlukan dalam pengujian
 
 5. Pilih file data berformat .xlsx yang ingin di analisis 
    dengan cara klik tombol 'IMPORT'
 
 6. Setelah data berhasil diimport, data dapat dilihat melalui 
    pilihan 'LIHAT DATA'

 7. Output Hasil Uji Kruskall Wallis dapat diakses melalui 
    pilihan 'ANALISIS' dan output akan otomatis keluar
                            
"))
  tombol2<-tkbutton(step2,text="Back to Help",command=function()tkdestroy(step2))
  tkgrid(tombol2)
  tkconfigure(teks2,state="disabled")
  tombollll<-tkbutton(step2,text="Next",command=function()stepp2())
  tkgrid(tombollll)
  tkconfigure(teks2,state="disabled")
  stepp2<-function()
  {
    require(tcltk)
    tkdestroy(step2)
    langkah2<-tktoplevel(bg="paleturquoise")
    tktitle(langkah2)<-"Langkah Kerja Program"
    scr3<-tkscrollbar(langkah2,orient="vertical",command=function(...)tkyview(teks3,...))
    teks3<-tktext(langkah2,bg="palegreen",width=80,height=12,yscrollcommand=function(...)tkset(scr3,...))
    tkgrid(teks3,scr3,sticky="news")
    tkgrid.rowconfigure(langkah2,teks3,weight=1)
    tkgrid.columnconfigure(langkah2,teks3,weight=1)
    tkinsert(teks3,"end",paste("
 ------ LANGKAH-LANGKAH PENGUJIAN KRUSKAL WALLIS DENGAN RINGKASAN DATA ------
 
 1. Beralih ke Menu Utama
 
 2. Pilih Menu 1 (Uji Kruskal Wallis)
 
 3. Pilih Sub-Menu Ringkasan Data
 
 4. Inputkan seluruh informasi yang diperlukan dalam pengujian
 
 5. Klik Analisis Data untuk menampilkan hasil

 6. Output Hasil Uji Kruskall Wallis akan otomatis keluar
                            
"))
    tombol12<-tkbutton(langkah2,text="Back to Help",command=function()tkdestroy(langkah2))
    tkgrid(tombol12)
    tkconfigure(teks3,state="disabled")
  }
} 

############################### MENU 3 SUB MENU 2 ################################

#-------------------------------- CONTOH SOAL -----------------------------------#

cs1<-function()
{
  require(tcltk)
  contoh1<-tktoplevel(bg="skyblue")
  tktitle(contoh1)<-"Contoh Soal"
  scr3<-tkscrollbar(contoh1,orient="vertical",command=function(...)tkyview(teks,...))
  teks<-tktext(contoh1,bg="powderblue",width=85,height=20,yscrollcommand=function(...)tkset(scr3,...))
  tkgrid(teks,scr3,sticky="news")
  tkgrid.rowconfigure(contoh1,teks,weight=1)
  tkgrid.columnconfigure(contoh1,teks,weight=1)
  tkinsert(teks,"end",paste("
 -------------------------- Contoh Soal 1 -------------------------------
   SOAL :
    Peneliti ingin meneliti berbagai jenis daging yang diolah menjadi baso.
    Jenis daging yang digunakan, yaitu daging sapi, daging ayam, daging kelinci
    dan daging domba.
    Terdapat 20 sampel pada penelitian ini.
    Peneliti ingin meneliti apakah ada perbedaan tingkat kesukaan baso dari segi 
    rasa pada beberapa jenis daging yang diamati.
    Data-datanya adalah sebagai berikut:
    (Data terlampir pada file excel)
    -----------------------------------------
   | Baso Daging Sapi    | 4 | 3 | 4 | 3 | 4 |
    -----------------------------------------
   | Baso Daging Ayam    | 2 | 2 | 4 | 3 | 2 |
    -----------------------------------------
   | Baso Daging Kelinci | 3 | 4 | 4 | 4 | 4 |
    -----------------------------------------
   | Baso Daging Domba   | 3 | 4 | 5 | 5 | 3 |
    -----------------------------------------
    Tabel 1. Data tingkat kesukaan rasa baso antara baso daging sapi, 
             baso daging ayam, baso daging kelinci, dan baso daging domba
             
    Sumber : https://core.ac.uk/download/pdf/291490103.pdf

    - Karena data berskala ordinal, tidak perlu dilakukan uji normalitas.
      Berikut ini Langkah-Langkah Menggunakan Impor Data :
        Uji Kruskal Wallis -> Impor Data -> Masukkan hipotesis dan alpha -> 
        Impor data sekaligus melakukan uji kruskal wallis melalui tombol 
        IMPOR DATA -> hasil analisis akan muncul pada kotak VIEW
    
    - Berdasarkan data tabel 1, secara manual diperoleh :
       K = 4
       N = 20
       n = 5
       Sigma kuadrat Rj = 11989.5
       Sigma T = 960
       
    - Langkah-Langkah menggunakan Ringkasan Data
        Uji Kruskal Wallis -> Ringkasan Data -> Masukan Hipotesis, K, N, n,
        Sigma kuadrat Rj, Sigma T, dan alpha -> Analisis Data -> hasil keluar
        
    - Hipotesis :
        H0 : Tidak terdapat perbedaan tingkat kesukaan rasa baso antara 
             baso daging sapi, baso daging ayam, baso daging kelinci, 
             dan baso daging domba
        H1 : Terdapat perbedaan tingkat kesukaan rasa baso antara baso 
             daging sapi, baso daging ayam, baso daging kelinci, dan 
             baso daging domba
           
    - Hasil Analisis :
          Hasil analisis tersebut menunjukkan daerah kritisnya menolak H0 
        apabila nilai statistik uji H lebih dari nilai pendekatan distribusi
        chi-square dengan derajat bebas K-1. Diperoleh nilai statistik uji H 
        sebesar 6.2651, sehingga keputusannya yaitu menerima H0 karena nilai
        statistik uji H (6.2651) kurang dari nilai pendekatan distribusi 
        chi-kuadrat berderajat bebas 3 yaitu (7,81472).Dengan demikian 
        didapatkan kesimpulan bahwa tidak terdapat perbedaan tingkat kesukaan rasa 
        baso anatara beberapa jenis daging (baso daging sapi, baso daging ayam, 
        baso daging kelinci, dan baso daging domba).

"))
  tombol<-tkbutton(contoh1,text="Next",command=function()cs2())
  tkgrid(tombol)
  tkconfigure(teks,state="disabled")
  tomboll<-tkbutton(contoh1,text="Back to Menu", command=function()tkdestroy(contoh1))
  tkgrid(tomboll)
  tkconfigure(teks,state="disabled")
  
  cs2<-function()
  {
    tkdestroy(contoh1)
    require(tcltk)
    contoh2<-tktoplevel(bg="palegoldenrod")
    tktitle(contoh2)<-"Contoh Soal"
    t1<-tkbutton(contoh2,text="Back",command=function()cs1())
    tkgrid(t1)
    t4<-tkbutton(contoh2,text="Next",command=function()cs3())
    tkgrid(t4)
    scr<-tkscrollbar(contoh2,orient="vertical",command=function(...)tkyview(teks,...))
    teks<-tktext(contoh2,bg="palegreen",width=85,height=20,yscrollcommand=function(...)tkset(scr,...))
    tkgrid(teks,scr,sticky="news")
    tkgrid.rowconfigure(contoh2,teks,weight=1)
    tkgrid.columnconfigure(contoh2,teks,weight=1)
    tkinsert(teks,"end",paste("

 ------------------------------Contoh Soal 2----------------------------------   
  SOAL :
   Seorang mahasiswa prodi Ekonomi Syariah melakukan penelitian untuk 
   skripsinya dengan tujuan untuk mengetahui perbedaan Equitable Distribution 
   Ratio Beban Gaji berdasarkan beberapa Perbankan Syariah di ASEAN 
   (BMI, BSM, BIMB, BMMB, BIBD, IBT dan AIBP). Untuk keperluan tersebut 
   diambil sampel sejak tahun 2013-2016 dengan data sebagai berikut :
   (Data terlampir pada file excel)
    ----------------------------------------
   | BMI   | 35.67 | 40.52 | 38.98 | 49.33 |
    ----------------------------------------
   | BSM   | 26.42 | 33.39 | 31.26 | 30.47 |
    ----------------------------------------
   | BIMB  | 35.19 | 35.11 | 35.29 | 34.72 |
    ----------------------------------------
   | BMMB  | 37.27 | 37.28 | 45.44 | 36.32 | 
    ----------------------------------------
   | BIBD  | 24.69 | 26.23 | 21.92 | 18.89 |
    ----------------------------------------
   | IBT   | 46.77 | 52.80 | 32.67 | 35.39 |
    ----------------------------------------
   | AIBP  | 248.9 | 246.59| 218.71| 221.84|
    ----------------------------------
   Tabel 2. Data Equitable Distribution Ratio Beban Gaji berdasarkan beberapa 
            Perbankan Syariah di ASEAN (BMI, BSM, BIMB, BMMB, BIBD, IBT dan AIBP)
   Sumber : 
   https://repository.uinjkt.ac.id/dspace/bitstream/123456789/40697/1/AGUNG%20MAULANA-FEB.pdf

   - Karena Data berskala nominal, maka akan menguji normalitas terlebih dulu
     dengan langkah-langkah sebagai berikut :
       Uji Kruskal-Wallis -> Normalitas -> Masukkan alpha -> Impor data sekaligus 
       melakukan uji kruskal wallis melalui tombol IMPOR DATA -> hasil analisis 
       akan muncul pada kotak VIEW
   
   - Langkah Melakukan Uji Kruskal-Wallis :
        Uji Kruskal Wallis -> Impor Data -> Masukkan hipotesis dan alpha -> 
        Impor data sekaligus melakukan uji kruskal wallis melalui tombol 
        IMPOR DATA -> hasil analisis akan muncul pada kotak VIEW
        
   - Hipotesis :
        H0 : Tidak terdapat perbedaan Equitable Distribution Ratio Beban Gaji 
             antara beberapa Perbankan Syariah di ASEAN (BMI, BSM, BIMB, BMMB, 
             BIBD, IBT dan AIBP)
        H1 : Terdapat perbedaan Equitable Distribution Ratio Beban Gaji 
             antara beberapa Perbankan Syariah di ASEAN (BMI, BSM, BIMB, BMMB, 
             BIBD, IBT dan AIBP)
           
   - Hasil Analisis :
          Berdasarkan Hasil analisis uji normalitas data tersebut menunjukkan 
        daerah kritisnya menolak H0 apabila nilai signifikansi kurang dari 
        nilai alpha 0,05. Diperoleh nilai signifikansi sebesar 0,000 dan 
        statistik uji sebesar 0,531, sehingga keputusannya yaitu menolak H0
        karena nilai signifikansi (0,000) kurang dari nilai alpha (0,05). 
        Dengan demikian didapatkan kesimpulan bahwa data tabel 3 tidak 
        berdistribusi normal, sehingga data ini sudah memenuhi asumsi jika 
        digunakan analisis uji kruskal wallis. Selanjutnya, akan dilakukan 
        uji kruskal wallis.
          Hasil analisis uji kruskal wallis dengan program menunjukkan daerah 
        kritisnya menolak H0 apabila nilai statistik uji lebih dari nilai 
        pendekatan distribusi chi-square dengan derajat bebas K-1. Diperoleh 
        nilai statistik uji H sebesar 23,5345 dan signifikansi sebesar 0,000, 
        sehingga keputusannya yaitu menolak H0 karena nilai statistik uji H 
        (23,5345) lebih dari nilai pendekatan distribusi chi-kuadrat berderajat 
        bebas 6 yaitu (12,5916). Diperoleh kesimpulan bahwa terdapat perbedaan
        Equitable Distribution Ratio Beban Gaji pada beberapa Perbankan Syariah 
        di ASEAN (BMI, BSM, BIMB, BMMB, BIBD, IBT dan AIBP).
 "))
    t2<-tkbutton(contoh2,text="Back to Menu",command=function()tkdestroy(contoh2))
    tkgrid(t2)
    tkconfigure(teks,state="disabled")
   
  cs3<-function()
  {
    tkdestroy(contoh2)
    require(tcltk)
    contoh3<-tktoplevel(bg="powderblue")
    tktitle(contoh3)<-"Contoh Soal"
    t5<-tkbutton(contoh3,text="Back",command=function()cs2())
    tkgrid(t5)
    scr1<-tkscrollbar(contoh3,orient="vertical",command=function(...)tkyview(teks,...))
    teks<-tktext(contoh3,bg="pink",width=85,height=20,yscrollcommand=function(...)tkset(scr1,...))
    tkgrid(teks,scr1,sticky="news")
    tkgrid.rowconfigure(contoh3,teks,weight=1)
    tkgrid.columnconfigure(contoh3,teks,weight=1)
    tkinsert(teks,"end",paste("
    
     ------------------------------Contoh Soal 3----------------------------------   
      SOAL :
       Sampai saat ini, di wilayah Jawa Timur masih terdapat kasus Covid-19. 
       Kasus di wilayah ini masih tergolong rendah sampai sedang, untuk itu 
       penulis ingin menganalisis apakah terdapat perbedaan kasus terkonfirmasi 
       positif Covid-19 pada wilayah Surabaya Raya. Untuk keperluan 
       tersebut diambil sampel dari website resmi Covid di Jawa Timur terhitung 
       mulai tanggal 2 Juni 2021 - 8 Juni 2021. Data terlampir pada file excel.
        -----------------------------------------------------------------------
       | KOTA SURABAYA | 24115 | 24140 | 24170 | 24198 | 24227 | 24249 | 24274 |
        -----------------------------------------------------------------------
       | KAB. GRESIK   |  5646 |  5648 |  5649 |  5656 |  5660 |  5666 |  5675 |
        -----------------------------------------------------------------------
       | KAB. SIDOARJO | 11476 | 11476 | 11491 | 11497 | 11507 | 11519 | 11530 |
        -----------------------------------------------------------------------
       Tabel 3. Data kasus terkonfirmasi positif Covid-19 di beberapa wilayah
                di Jawa Timur
             
       Sumber : http://infocovid19.jatimprov.go.id/
    
       - Karena Data berskala nominal, maka akan menguji normalitas terlebih dulu
         dengan langkah-langkah sebagai berikut :
           Uji Kruskal-Wallis -> Normalitas -> Masukkan alpha -> Impor data sekaligus 
           melakukan uji kruskal wallis melalui tombol IMPOR DATA -> hasil analisis 
           akan muncul pada kotak VIEW
   
      - Langkah Melakukan Uji Kruskal-Wallis :
           Uji Kruskal Wallis -> Impor Data -> Masukkan hipotesis dan alpha -> 
           Impor data sekaligus melakukan uji kruskal wallis melalui tombol 
           IMPOR DATA -> hasil analisis akan muncul pada kotak VIEW
        
      - Hipotesis :
           H0 : Tidak terdapat perbedaan kasus terkonfirmasi positif Covid-19 di 
                wilayah Surabaya Raya
           H1 : Terdapat perbedaan kasus terkonfirmasi positif Covid-19 di 
                wilayah Surabaya Raya
           
      - Hasil Analisis :
          Berdasarkan Hasil analisis uji normalitas data tersebut menunjukkan 
        daerah kritisnya menolak H0 apabila nilai signifikansi kurang dari 
        nilai alpha 0,05. Diperoleh nilai signifikansi sebesar 0,000 dan 
        statistik uji sebesar 0,767, sehingga keputusannya yaitu menolak H0
        karena nilai signifikansi (0,000) kurang dari nilai alpha (0,05). 
        Dengan demikian didapatkan kesimpulan bahwa data tabel 3 tidak 
        berdistribusi normal, sehingga data ini sudah memenuhi asumsi jika 
        digunakan analisis uji kruskal wallis. Selanjutnya, akan dilakukan 
        uji kruskal wallis.
          Hasil analisis uji kruskal wallis dengan program menunjukkan daerah 
        kritisnya menolak H0 apabila nilai statistik uji lebih dari nilai 
        pendekatan distribusi chi-square dengan derajat bebas K-1. Diperoleh 
        nilai statistik uji H sebesar 61,2809, sehingga keputusannya yaitu 
        menolak H0 karena nilai statistik uji H (17.8182) lebih dari nilai 
        pendekatan distribusi chi-kuadrat berderajat bebas 4 yaitu (5.9915). 
        Diperoleh kesimpulan bahwa terdapat perbedaan kasus terkonfirmasi 
        positif Covid-19 di wilayah Surabaya Raya.
     "))
    t6<-tkbutton(contoh3,text="Back to Menu",command=function()tkdestroy(contoh3))
    tkgrid(t6)
    tkconfigure(teks,state="disabled")
  }
}
}
