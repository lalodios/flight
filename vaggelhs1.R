Sys.setlocale("LC_CTYPE","Greek")
library(dplyr)

mydata<-read.csv("DelayedFlights.csv",header=TRUE)
class(mydata)
View(mydata)


#Ερώτηση 1:να βρείτε (αν υπάρχουν) και να εμφανίσετε το πλήθος των κενών γραμμών σε κάθε στήλη του dataset
####Άρα για κάθε μεταβλητή μου θα πρέπει να εμφανήσω το πλήθος των γραμμών που έχουν ΝΑ.
mylist<-list("Month"=mydata$Month,mydata$DayofMonth,mydata$DayOfWeek,mydata$DepTime,mydata$CRSDepTime,mydata$ArrTime,
             mydata$CRSArrTime, mydata$UniqueCarrier,mydata$FlightNum,mydata$TailNum,mydata$ActualElapsedTime,
             mydata$CRSElapsedTime, mydata$AirTime,mydata$ArrDelay,mydata$DepDelay,mydata$Origin,mydata$Dest,
             mydata$Distance, mydata$TaxiIn,mydata$TaxiOut,mydata$Cancelled,mydata$CancellationCode,
             mydata$Diverted,mydata$CarrierDelay,mydata$WeatherDelay,mydata$NASDelay,mydata$SecurityDelay,
             mydata$LateAircraftDelay)

SUM<-0
i<-0
w<-0
for(i in 1:length(mylist))
{
  NALINES<-as.vector(is.na(mylist[[i]]))
  {
    w<-0
    SUM<-0
    for(w in length(NALINES):1 )
    {
      if(isTRUE(NALINES[w]==TRUE))
      {
        SUM<-SUM+1
      }
    }
  }
  print(i)
  print(SUM)
}

#Ερώτηση 2:να υπολογίσετε και να εμφανίσετε ποια ημέρα σε ποιον μήνα σημειώθηκαν οι περισσότερες καθυστερήσεις πτήσεων

C<-mydata$CRSArrTime
delay<-numeric(length(C))
for(i in length(mydata$CRSArrTime):1)
{
  if(isTRUE(mydata$ArrTime[i]-mydata$CRSArrTime[i]>0))
  {
    delay[i]<-mydata$ArrTime[i]-mydata$CRSArrTime[i]
  }
}
delay

delay<-as.data.frame(delay)
mydata<-cbind(mydata,delay)
View(mydata)

by_Month<-mydata%>%select(Month,delay,mydata$DayofMonth)
by_Month<-by_Month%>%group_by(Month,DayofMonth)
by_Month<-by_Month%>%summarise(delay=sum(delay))
by_Month<-by_Month%>%arrange(desc(delay))
View(by_Month)
by_Month[1,1:3]

"αρα οι περισσοτερες καθηστερησεις σημειωθηκαν τον 12ο μήνα την 19η μέρα "


#Ερώτηση 3: να υπολογίσετε και να εμφανίσετε τον ημερήσιο μέσο όρο καθυστερήσεων 
#για καθέναν από τους θερινούς μήνες του 2008


mo_6<-filter(mydata,Month==6)%>%
  group_by(DayofMonth,Month)
w2<-summarise(mo_6,m1=mean(delay,na.rm=TRUE))
View(w2)

mo_7<-filter(mydata,Month==7)%>%
  group_by(DayofMonth,Month)
w3<-summarise(mo_7,m1=mean(delay,na.rm=TRUE))
View(w3)

m0_8<-filter(mydata,Month==8)%>%
  group_by(DayofMonth,Month)
w4<-summarise(m0_8,m1=mean(delay,na.rm=TRUE))
View(w4)

#Ερώτηση 4: να υπολογίσετε και να εμφανίσετε το όνομα της αεροπορικής εταιρίας
#που είχε το μεγαλύτερο πλήθος κωδικών ακύρωσης τύπου Β

" το όνομα της αεροπορικης εταιρίας είναι uniqueCarrier"

ka8isterisi<- mydata%>%
  select(UniqueCarrier,CancellationCode)%>%
  filter(CancellationCode=="B")%>%
  group_by(UniqueCarrier)%>%
  count()%>%
  arrange(desc(n))
ka8isterisi<-as.data.frame(ka8isterisi)
ka8isterisi[1,1:2]
 "επομέμως το όνομα της αεροποτικης εταιρίας είναι το MQ και είχε 58 ακυρωθήσες κλήσεις τύπου Β"
 
 #Ερώτηση 5: να βρείτε τους κωδικούς των πτήσεων με τον μεγαλύτερο αριθμό καθυστερήσεων
 
 kod_ka8is<-mydata%>%
   select(FlightNum,delay)%>%
   group_by(FlightNum)%>%
   summarise(sum=sum(delay))%>%
   arrange(desc(sum))
head(kod_ka8is)

"επομένως ο αριθμός πτήσης με τα περισσότερα λεπτά καθηστερήσεων ειναι ο 75 "


   #Ερώτηση 6: να βρείτε και να υπολογίσετε το όνομα του μεγαλύτερου σε απόσταση 
   # προορισμού με τις περισσότερες καθυστερήσεις



proorismos<-mydata%>%
   select(Dest,Distance,Origin)%>%
  group_by(Origin,Dest)%>%
   arrange(desc(Distance))
"παρατηρώ ότι ο μεγαλυτεροσ σε αποσταση προορισμος ειναι ο HNL αρα θα παρω 
τισ πτησεις που ειχαν μονο αυτον τον προορισμο"

proorismos<-mydata%>%
  select(Origin,Dest,Distance,delay)%>%
  filter(Dest=="HNL")%>%
  group_by(Origin)%>%
  summarise(sum=sum(delay))%>%
  arrange(desc(sum))
  
  View(proorismos)
proorismos[1,2:1]

"επομένως παρατηρω οτι ο ποιο απομακρυσμενος προορισμος ειναι ο HNL 
αλλα οι περισσοτερες καθηστερησεις εγιναν απο το αεροδρομιο με αναχωρηση απο
το LAX."


#Ερώτηση 7: να βρείτε και να εμφανίσετε τους προορισμούς που είχαν 
#την μεγαλύτερη καθυστέρηση (πτήσεις που εκτελέστηκαν)

proo_ektel<-mydata%>%
  select(Dest,Cancelled,delay)%>%
  filter(Cancelled=="0")%>%
  group_by(Dest)%>%
  arrange(desc(delay))
head(proo_ektel)
 " επομέμως οι προορισμοι με την μεγαλυτερη καθηστέρηση ειναι οι EWR,ORD,PBI,FLL"


 #Ερώτηση 8: να βρείτε και να εμφανίσετε το όνομα της αεροπορικής εταιρείας που είχε τις μεγαλύτερες καθυστερήσεις
 #που οφείλονται σε καθυστερημένη άφιξη αεροσκαφών

 aer_eter<-mydata%>%
   select(LateAircraftDelay,UniqueCarrier)%>%
   group_by(UniqueCarrier)%>%
   summarise(sum=sum(LateAircraftDelay,na.rm=TRUE))%>%
   arrange(desc(sum))
head(aer_eter)
 
"Αρα η αεροπορικη εταιρια με την μεγαλυτερη καθηστεριση εξαιτιας καθηστεριμενης αφιξης αεροσκαφων ειναι η WN."

#Ερώτηση 9: να υπολογίσετε πόσες ακυρώσεις πτήσεων τύπου Α σημειώθηκαν την 13η ημέρα κάθε μήνα

mera13<-mydata%>%
  select(CancellationCode,Month,DayofMonth)%>%
  filter(CancellationCode=="A")%>%
  filter(DayofMonth=="13")%>%
mera13
 "Ακυρώσεις τύπου Α είχα μόνο τους μήνες 11ο και 12ο"  



 #Ερώτηση 10: υπολογίσετε και να εμφανίσετε την μέση καθυστέρηση πτήσεων που 
 #εκτελέστηκαν από την 10η μέχρι την 23 Απριλίου 2008

 mhnas_1<-mydata%>%
   select(delay,Month,DayofMonth,Cancelled)%>%
   filter(Cancelled=="0")%>%
   filter(Month=="4")%>%
   filter(between(DayofMonth,10,23))%>%
  summarise(mean=mean(delay,is.na=TRUE))
 View(mhnas_1)
 mhnas_1
 
 
 " η μέση καθυστέρηση πτήσεων είναι 53,2"
 
 
 #Ερώτηση 11: να υπολογίσετε και να εμφανίσετε τον μήνα που σημειώθηκε η μεγαλύτερη καθυστέρηση
 #που οφειλόταν σε έλεγχους ασφαλείας κατά τις ώρες 06.00-14.00
 
 
 elegxos=mydata%>%
   select(Month,SecurityDelay,CRSDepTime,DepDelay)%>%
 group_by(Month)%>%
 filter(DepDelay>0)%>%
 filter( CRSDepTime <=1400 )%>%
 filter( CRSDepTime >=0600)%>%
 group_by(Month)%>%
 summarise(sum=sum(SecurityDelay,na.rm=TRUE))%>%
   arrange(desc(sum))
           
 View(elegxos)        
 
 "αρα ο μηνας με την μεγαλυτερη καθυστερηση ειναι ο Μαρτιος
 με συνολικη καθυστερηση 7973 λεπτά"
 
 #Ερώτηση 12: να υπολογίσετε και να εμφανίσετε ποιος κωδικός πτήσης(αριθμός πτήσης) είχε το πρώτο δεκαήμερο του
 #Νοεμβρίου του 2008την μεγαλύτερη προ του αναμενόμενου χρόνου άφιξη στον προορισμό της
 
 C<-mydata$CRSArrTime
 delayno<-numeric(length(C))
 for(i in length(mydata$CRSArrTime):1)
 {
   if(isTRUE(mydata$ArrTime[i]-mydata$CRSArrTime[i]<0))
   {
     delayno[i]<-mydata$ArrTime[i]-mydata$CRSArrTime[i]
   }
 }
 delayno
 
 afixi<-mydata %>%
   select(Month,DayofMonth,FlightNum)
 afixi<-cbind(afixi,delayno)
 afixi<-afixi%>%
   filter(Month=="11")%>%
   filter(DayofMonth > 0)%>%
  filter(DayofMonth < 11)%>%
  group_by(FlightNum)%>%
  arrange(desc(delayno))
 tail(afixi)
 
 
 " ο κωδικος πτήσης που εφτασε ποιο νωρις στον προορισμο του ηταν o 209 "
 
#Ερώτηση 13: να υπολογίσετε και να εμφανίσετε ποιο αεροδρόμιο (τοποθεσία αναχώρησης)
#είχε το δεύτερο δεκαήμερο του Αυγούστου 2018 τις περισσότερες πτήσεις
#με καθυστέρηση(αναχωρίσεων)μεγαλύτερη από μισή ώρα που οφείλονται στους αερομεταφορείς
 
 anaxorisi<-mydata %>%
   select(Month,DayofMonth,Origin,delay,CarrierDelay,ArrDelay)%>%
   
 filter(Month=="8")%>%
 filter(DayofMonth > 11)%>%
 filter(DayofMonth < 21)%>%
 filter(CarrierDelay>30)%>%
 filter(ArrDelay>0)%>%
 group_by(Origin)%>%
  count()%>%
 arrange(desc(n))
 anaxorisi[1,1:2]
 
 " τοποθεσια ATL ΜΕ 515 ΠΤΗΣΕΙΣ"
 
 #Ερώτηση 14: να βρείτε και να εμφανίσετε τις πτήσεις που εκτράπηκαν από την πορεία τους
 #αλλά ολοκληρώθηκαν καθώς και τον συνολικό χρόνο που απαιτήθηκε
 
 ektropi<-mydata%>%
   select(FlightNum,Diverted,Cancelled,ActualElapsedTime)
 ektropi<- as.data.frame(ektropi)
 ektropi<-ektropi%>%filter(Diverted=="1")
 ektropi<-ektropi%>%filter(Cancelled=="0")
 View(ektropi)
############
 #Ερώτηση 15: ποιος μήνας είχε την μεγαλύτερη τυπική απόκλιση σε καθυστερήσεις ("πιο απρόβλεπτος μήνας"). Ως απόκλιση να θεωρηθεί η διαφορά
 #ανάμεσα στον προγραμματισμένο και τον πραγματικό χρόνο εκτέλεσης της πτήσης
 
 
 C<-mydata$CRSArrTime
 delayno<-numeric(length(C))
 for(i in length(mydata$CRSArrTime):1)
 {
   delayno[i]<-mydata$ArrTime[i]-mydata$CRSArrTime[i]
 }
 delayno<-as.numeric(delayno)
 
 
 apoklisi<-mydata%>%
   select(Month,ArrTime)
 apoklisi<-as.data.frame(apoklisi)
 delayno<-as.data.frame(delayno)
 apoklisi<-cbind(apoklisi,delayno)
 apoklisi<-apoklisi%>%group_by(Month)
 apoklisi<-na.omit(apoklisi)
 View(apoklisi)
 apoklisi<-apoklisi%>%summarise(sd=sd(delayno),na.rm=TRUE)
 View(apoklisi)
 apoklisi<-apoklisi%>%arrange(desc(sd))
 apoklisi[1,1:2]
" Ο ΜΗΝΑς ΙΟΥΛΙΟς "
 
 
 