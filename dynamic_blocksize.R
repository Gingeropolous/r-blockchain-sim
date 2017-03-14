# Monero dynamic blocksize simulator script
# 2017 Gingeropolous

# install.packages("ggplot2")
# install.packages("reshape")
# install.packages("digest")

library(digest)
require(ggplot2)
require(reshape)

setwd("/home/user/Desktop/")
sink('blocksim_output.txt')

# Number of blocks to run simulation
num_blocks <- 100

default_mult <- 4 # The default fee multiplier
size_block_template <- 0 # Don't modify
wallet_auto_fee <- FALSE # When I figure out how to code the auto fee I'll play with it
gen_coins <- 14092278e12 # Total generated coins grabbed from moneroblocks.info circa 3/12/2017
tx_size <- 13*1024 # A fixed transaction size. can be made variable at some point
tot_coins <- 0xffffffffffffffff # Grabbed from moneromooos thing. Hooray R can read this nonsense

# Create a tx_pool, where each transaction is merely identified by the fee multiplier associated with it
# Transactions will be added to the pool by appending to it, so the higher index of the transaction, the newer the transaction
# tx_pool <- c() # should declare empty? screw it, we're not concerned with memory performance
#I think this should be a matrix

tx_pool <- matrix(NA,ncol = 4)

# Col 1: block entered
# Col 2: fee multiplier
# Col 3: hash
# Col 4: tx_size, currently fixed, but will at some point be variable

# Create a blockchain, and boostrap it with a median as pulled from the existing blockchain
blockchain <- matrix(NA,nrow=200, ncol = 9)
blockchain[,1] <- (rnorm(200, mean=51983.5, sd=20000))
hist(blockchain[,1])

# Col 1: block size
# Col 2: Number of transactions
# Col 3: unmodified base reward
# Col 4: Penalty
# Col 5: Total fees
# Col 6: Final block reward
# Col 7: Number of txs in txpool
# Col 8: Num transactions entered into txpool
# Col 9: Penalized Block Reward

# bcnames <- c("Block Size","Transactions in block","Base Reward","Block Penalty","Fees in Block","Total Block Reward","Number of txs in txpool","Num transactions entered into txpool","Penalized Block Reward","nums")


#Formulas

#From: https://en.wikipedia.org/wiki/Monero_(cryptocurrency)#Features
# max(0.6, floor((tot_coins − gen_coins)*2e−19)*10e−12)
# max(0.6, floor((M − A)×2−19)×10−12) XMR, with M = 264 − 1 and A = 1012 times the amount of XMR already emitted).


#From: http://monero.stackexchange.com/questions/2531/how-does-the-dynamic-fee-calculation-work
#fee = (R/R0) * (M0/M) * F0
#R: base reward
#R0: reference base reward (10 monero)
#M: block size limit
#M0: minimum block size limit (60000)
#F0: 0.002 monero

#From: https://github.com/monero-project/monero/pull/1276/files#diff-1279d7b0ddc432573cd2bd8c6e632c1fR2718
# uint64_t unscaled_fee_per_kb = (DYNAMIC_FEE_PER_KB_BASE_FEE * CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2 / median_block_size);

ref_base <- 10e12
CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2 <- 60000
twomed <- 2*CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2 # Will later be adaptive
fee_factor <- 0.004e12

# Our Transaction Number Distribution
num_transaction_dist <- floor(rnorm(400, mean=4, sd=3))
hist(num_transaction_dist)

for (i in 1:num_blocks) {
  print("Beginning of block loop")
  print(i)
  #Take care of base reward!
  base_reward <-( tot_coins - gen_coins ) * 2^-19 
  if (base_reward < 600000000000) { base_reward <- 600000000000 }
  
  #Lets try the per kb fee
  #The one below, as documented on the SE and on bitcointalk, doesn't seem to actually be implemented in the code
  #fee <- (base_reward/ref_base)*(min_block/block_size_limit)*fee_factor
  
  # Lets generate some fake numbers!
  
  # Number of transactions being created by users and added to the txpool in this block
  num_tx <- floor(rnorm(1, mean=4, sd=3)) # Genernate the number of transactions that are getting into the mempool during this block
  if (num_tx < 0 ) {num_tx <- 0} # If the numberis below 0, then force to 0. Skews the distribution to 0,
  if (i ==1 ) {num_tx <- 10} # Prevents dimension problem if first transaction pool is 0
  #print("Number of transactions")
  #print(num_tx)
  
  # What is the current size of the transaction pool
  # fee_ordered_pool[complete.cases(fee_ordered_pool),]
  # tx_pool_num <- nrow(tx_pool[complete.cases(tx_pool),])
  tx_pool_num <- nrow(tx_pool)
  tx_pool_size <- tx_pool_num * tx_size

  # What is the current median of the last 100 blocks
  size_of_blockchain <- nrow(blockchain)
  last_100 <- c(blockchain[(size_of_blockchain-99):size_of_blockchain],1)
  med_100 <- median(last_100)
  if (med_100 < CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2) {med_100 <- CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2}
  
  #This one seems to be implemented
  # unscaled_fee_per_kb = (DYNAMIC_FEE_PER_KB_BASE_FEE * CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2 / median_block_size);
  perkb_fee <- fee_factor*CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2/med_100
  
  # Lets try simulating transactions being created during this block    
  for (p in 1:num_tx) {  
    if (tx_pool_size > med_100 && wallet_auto_fee == TRUE) {
      # skip for now
    }
    else {
      transaction <- c(i,default_mult*perkb_fee*(tx_size/1024),digest(runif(1),algo="md5"),tx_size) # Add the transaction to the mempool, 
      # Where the transaction is identified by the block # its added in, the fee multiplier, a random hash, and the transaction size
      tx_pool <- rbind(tx_pool,transaction) 
    }
  }
  
  
  # OK, lets try to make a block and add it to the chain and delete the transactions from the transaction pool
  # This is gonna be a bitch
  
  tx_pool_copy <- tx_pool # Lets not mess with the main transaction pool
  block_template <- matrix(NA, ncol = 4)
  
  block_template[1,] <- c(0,0,digest(runif(1),algo="md5"),95.0272) # initiate the block template, creating a random hash, and the size comes from 0.0928*1024
  #print("Before blockfill loop")
  #print(block_template)
  
  countloop <- 0
  if (nrow(tx_pool) == 0) {print("Tx pool empty")}
  else {
  repeat { ### This is the beginning of the block template loop
    countloop <- countloop + 1
    
  # First, we'll see if there are any high fee transactions in the mempool
    
    if (nrow(tx_pool_copy) == 0) { break } # Introduced because buggy if txpool is empty
    else {
    fee_ordered_pool <- tx_pool_copy[sort.list(tx_pool_copy[,2],decreasing=TRUE),,drop=FALSE ]
    highest_fee <- as.numeric(fee_ordered_pool[1,2]) # find the highest fee
    high_fee_index <- c(which(as.numeric(fee_ordered_pool[,2]) == highest_fee)) # Get row indexes of all the transactions with this fee
    high_fee_pool <- rbind(fee_ordered_pool[high_fee_index,]) # Create sub matrix of the high transactions
    
    block_ordered_pool <- high_fee_pool[sort.list(high_fee_pool[,1]),,drop=FALSE]
    size_block_template <- sum(as.numeric(block_template[,4]))
    
    #print("After loop")
    #print(block_template)
    #block_ordered_pool <- high_fee_pool[order(high_fee_pool[,1],decreasing=FALSE),] # Sort based on block added - older transactions will be added first
    #print ("Size of block template")
    #print (size_block_template)
    # The below transaction adding logic misses any transactions that would fall below the standard size of a transaction
    
    if ((size_block_template + tx_size) <= med_100){ # So this is the easiest. If the block is lower than the median, we just add most recent and highest fee (would be the most profitable)
      #print(block_ordered_pool[1,]) # For logging
      block_template <- rbind(block_template,block_ordered_pool[1,]) # Add a transaction to the block template
      tx_pool_copy <- tx_pool_copy[-1,,drop = FALSE] # delete this transaction from the tx_pool_copy, drop=FALSe prevents the fucker from turning into a vector
      perc_penalty <- 0
      pnlz_block_reward <- base_reward
      
      newblocksize <- sum(as.numeric(block_template[,4]))
      newblocktx <- nrow(block_template)
      newblockbr <- base_reward
      newblockpnlt <- base_reward - pnlz_block_reward
      newblockfees <- sum(as.numeric(block_template[,2]))
      newblockreward <-pnlz_block_reward + newblockfees
      newbasereward <- pnlz_block_reward
      
      print("Size of candidate block less than median")
      print("Block template")
      print(block_template)
      print("Size of block template + new transaction")
      print(size_block_template+tx_size)
      print ("New block reward")
      print (newblockreward)
      
      }
    
    else if ((size_block_template + tx_size) > med_100) { # This is the bitch case where we have to figure out if going over the median makes any sense
      temp_diff <- (size_block_template + as.numeric(block_ordered_pool[1,4])) - med_100  # This just grabs the difference between the median 100 and the potential new block with an additional transaction. 
      perc_penalty <- temp_diff / med_100
      pnlz_block_reward <- base_reward - (base_reward * perc_penalty)
      if (pnlz_block_reward + (sum(as.numeric(block_template[,2]))) + as.numeric(block_ordered_pool[1,2]) > base_reward) {
        block_template <- rbind(block_template,block_ordered_pool[1,]) # Add a transaction to the block template
        tx_pool_copy <- tx_pool_copy[-1,,drop = FALSE] # delete this transaction from the tx_pool_copy, drop=FALSe prevents the fucker from turning into a vector
        
        print("Size of candidate block GREATER than median")
        newblocksize <- sum(as.numeric(block_template[,4]))
        newblocktx <- nrow(block_template)
        newblockbr <- base_reward
        newblockpnlt <- base_reward - pnlz_block_reward
        newblockfees <- sum(as.numeric(block_template[,2]))
        newblockreward <-pnlz_block_reward + newblockfees
        newbasereward <- pnlz_block_reward
        
        print("Block template")
        print(block_template)
        print("Size of block template + new transaction")
        print(size_block_template+tx_size)
        print ("New block reward")
        print (newblockreward)
        
        }
      else { break} # The placement of this break is presumably what caused the blockformation stuff to be borked if it was after the if-tree
    }
    else {print("WTF")}
    #print("Start of blockfill loop stats:")
    #print(tx_pool_copy)
    #print(block_template)
    #print("End of stats")
    }
 } ##### This is the end of the Repeat Loop for creating a block template
} ## end of the else for the tx_pool being full
  
  print("We made it out of the blockfill loop")
  # Col 1: block size
  # Col 2: Number of transactions
  # Col 3: unmodified base reward
  # Col 4: Penalty
  # Col 5: Total fees
  # Col 6: Final block reward
  
  # Lets add a new block to the chain !!!
  newblock <- c(newblocksize,newblocktx,newblockbr,newblockpnlt,newblockfees,newblockreward,nrow(tx_pool_copy),num_tx,newbasereward)
  
  blockchain <- rbind (blockchain,newblock)
  
  tx_pool <- tx_pool_copy
  #print("Iteration")
  #print(i)
  #print("New Block Info")
  #print(newblock)
  
  gen_coins <- gen_coins + newbasereward 
  
  # Rest all these variables
  newblocksize <- 0
  newblocktx <- 0
  newblockbr <- 0
  newblockpnlt <- 0
  newblockfees <- 0
  newblockreward <-0
  newbasereward <- 0
} ############# End of primary for loop

synth_blockchain <- blockchain[complete.cases(blockchain),]

bcnames <- c("Block Size","Transactions in block","Base Reward","Block Penalty","Fees in Block","Total Block Reward with Fees","Number of txs in txpool","Num transactions entered into txpool","Final Base Reward")

#matplot(synth_blockchain,type="l",)
#legend("bottom", inset=.05, legend=bcnames, pch=1, col=c(2,4), horiz=FALSE)

nums <- seq(1,nrow(synth_blockchain),1)
#synth_blockchain <- cbind(synth_blockchain,nums)
colnames(synth_blockchain)<-bcnames
bcdf <- data.frame(synth_blockchain,row.names=nums)
bcdf


sink()


write.table(bcdf, file = "blockchain.txt", append = FALSE, quote = TRUE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

#plot(synth_blockchain[,1], type="o", col="blue")



