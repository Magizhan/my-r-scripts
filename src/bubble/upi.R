library(googlesheets)
library(bigrquery)
library(tidyverse)
library(lubridate)
library(tibbletime)
library(plyr)
library(esquisse)

sql <- "
select substr(ec.date_created,1,7) as mon, ec.payment_method_type as pmt,ec.source_object as upi_type, card_type, ec.merchant_id as mid, ec.gateway as pg, sum(ec.isOrderSucc) as success, sum(ec.orderTotal) as tot
from
(select 1 as orderTotal, source_object, amount,(CASE WHEN status = 'CHARGED' THEN 1 ELSE 0 END) as isOrderSucc,(CASE WHEN txn_conflict = 'CONFLICTED' THEN 1 ELSE 0 END) as isDelayed, (CASE WHEN txn_conflict = 'CONFLICTED' and status='CHARGED' THEN 1 ELSE 0 END) as isConflicted,date_created, merchant_id,using_stored_card, customer_id, order_uuid, payment_method_type,order_id,gateway,  card_type,error_message, resp_message ,status,card_switch_provider, card_issuer_bank_name,udf2,udf3,udf1,txn_last_modified, order_last_modified,amount_refunded from TABLE_DATE_RANGE(express_checkout_v2.express_checkout,TIMESTAMP('2018-09-01'),CURRENT_TIMESTAMP())) as ec
group by mon,mid,pg,pmt, upi_type, card_type "
project <- "godel-big-q"

qry<-function(x){
  bq_project_query(project, query=x, use_legacy_sql = TRUE, max_pages = Inf, location="asia-south1", destination_table=NULL, default_dataset = NULL) %>% bq_table_download(max_results = Inf) %>% as_tibble()
}
df <- qry(sql)

# Creating Channel by adding Google pay to UPI and distinguishing credit and debit among cards.
df <- df %>% filter (!(mid =="<NA>" | mon=="<NA>" | pg=="<NA>" | pmt=='<NA>')) %>% mutate (channel = if_else(pmt=="UPI"|(pg=="GOOGLEPAY"),"UPI",pmt) )
df <- df %>% mutate(channel = if_else(card_type=="CREDIT","CREDIT_CARD",channel))
df <- df %>% mutate(channel = if_else(card_type=="DEBIT","DEBIT_CARD",channel))
df <- df %>% mutate(channel = if_else(is.na(channel),pmt,channel))

#calculate sr
dfs <- df %>% dplyr::group_by(channel,mid, mon,pg) %>% dplyr::summarise(tot=sum(tot),SR=round(sum(success)/sum(tot)*100,2)) %>% select (mid,mon,pg,channel,tot,SR) 
#calculate proportion
dfsp <- dfs %>% plyr::ddply(.(mon,mid),transform, prop=round(tot/sum(tot)*100,2))


# Extract data for Bubble Chart Movie
dfsb <- dfsp %>% filter (channel=="UPI" & tot>500) %>% select (mid,pg,mon,tot,prop,SR) %>% mutate (mon=as.yearmon(paste0(mon,"-01")),mid=as.factor(mid)) %>% as_tibble()
saveRDS(dfsb,"/Users/magizhan.selvan/Documents/code/x-ray/src/bubble/upi.Rds")
dfsb2 <- readRDS("/Users/magizhan.selvan/Documents/code/x-ray/src/bubble/upi.Rds")
identical(dfsb,dfsb2)



# Bubble Chart Extensions for other payment methods

dfsb_dc <- dfsp %>% filter (channel=="DEBIT_CARD" & tot>500) %>% select (mid,pg,mon,tot,prop,SR) %>% mutate (mon=as.yearmon(paste0(mon,"-01")),mid=as.factor(mid)) %>% as_tibble()

dfsb_cc <- dfsp %>% filter (channel=="CREDIT_CARD" & tot>500) %>% select (mid,pg,mon,tot,prop,SR) %>% mutate (mon=as.yearmon(paste0(mon,"-01")),mid=as.factor(mid)) %>% as_tibble()

dfsb_nb <- dfsp %>% filter (channel=="NB" & tot>500) %>% select (mid,pg,mon,tot,prop,SR) %>% mutate (mon=as.yearmon(paste0(mon,"-01")),mid=as.factor(mid)) %>% as_tibble()

dfsb_wlt <- dfsp %>% filter (channel=="WALLET" & tot>500) %>% select (mid,pg,mon,tot,prop,SR) %>% mutate (mon=as.yearmon(paste0(mon,"-01")),mid=as.factor(mid)) %>% as_tibble()

saveRDS(dfsb_dc,"/Users/magizhan.selvan/Documents/code/x-ray/src/bubble/dc.Rds")
saveRDS(dfsb_cc,"/Users/magizhan.selvan/Documents/code/x-ray/src/bubble/cc.Rds")
saveRDS(dfsb_nb,"/Users/magizhan.selvan/Documents/code/x-ray/src/bubble/nb.Rds")
saveRDS(dfsb_wlt,"/Users/magizhan.selvan/Documents/code/x-ray/src/bubble/wlt.Rds")




# Misc Graphs & Analysis

dfs1 <- df %>% filter(mon>"2019-11") %>% group_by(channel,pg,mid) %>% dplyr::summarise(tot=sum(tot),SR=round(sum(success)/sum(tot)*100,2)) %>% select (channel,SR,tot,mid,pg) %>% ddply(.(mid),transform, prop=round(tot/sum(tot)*100,2))
dfs %>% filter (mid=="olacabs" | mid=="dream11" | mid=="dreamplug_live") %>% ggplot(mapping=aes(mon,prop,size=SR,color=channel))+ facet_wrap(~mid, ncol=1) + geom_point()  +theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
dfs %>% filter (mid=="mplgaming" | mid=="MVA_POSTPAID" | mid=="MVA_PREPAID") %>% ggplot(mapping=aes(mon,prop,size=SR,color=channel))+ facet_wrap(~mid, ncol=1) + geom_point()  +theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
dfs %>% filter (mid=="dream11") %>% ggplot(mapping=aes(mon,prop,size=SR,color=channel))+ facet_wrap(~mid, ncol=1) + geom_point()  +theme(axis.text.x = element_text(angle = 90, hjust = 1)) 
