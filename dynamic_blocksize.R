# Monero dynamic blocksize simulator script
# 2017 Gingeropolous

# install.packages("ggplot2")
# install.packages("reshape")

#Only this one is necessary. Tried the above to to try and get some graphs but lost patience. 
# install.packages("digest")

library(digest)
#library(adagio)
# require(ggplot2)
# require(reshape)

setwd("/home/user/Desktop/r-blockchain-sim/")
# sink('blocksim_output.txt')


# PARAMETERS TO MODIFY
tx_mean <- 20 # mean number of transactions to add to txpool during each block
tx_sd <- 5 # standard deviation for that 
# Our Transaction Number Distribution. Just run these lines to see the distribution
num_transaction_dist <- floor(rnorm(400, mean=tx_mean, sd=tx_sd))
hist(num_transaction_dist)

num_blocks <- 720 # Number of blocks to run simulation. 720 = 1 day
default_mult <- 4 # The default fee multiplier
spike_factor <- 3 # The multiplier of simulated transaction spikes that defy the normal distribution
wallet_auto_fee <- FALSE # When I figure out how to code the auto fee I'll play with it

gen_coins <- 14092278e12 # Total generated coins grabbed from moneroblocks.info circa 3/12/2017
plus_one <- FALSE # This triggers the experimental +1 policy, where there is no fee

# Formulas and parameters that can be modified 

ref_base <- 10e12
CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2 <- 60000 # new on is 300000, old one is 60000
fee_factor <- 0.004e12

# Write the new start of the blockchain text file
cat(c(date(),"New blockchain run","Number of blocks",num_blocks,"default multiplier",default_mult,"spike factor",spike_factor,"tx_mean",tx_mean,"tx_sd",tx_sd) , file = "live_blockchain.txt", sep = "\n", fill = FALSE, labels = NULL, append = FALSE)


#Formula sources

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

# These shouldn't be modified
size_block_template <- 0 # Don't modify
tot_coins <- 0xffffffffffffffff # Grabbed from moneromooos thing. Hooray R can read this nonsense

# Create a tx_pool, where each transaction is merely identified by the fee multiplier associated with it
# Transactions will be added to the pool by appending to it, so the higher index of the transaction, the newer the transaction
# tx_pool <- c() # should declare empty? screw it, we're not concerned with memory performance
#I think this should be a matrix

tx_pool <- matrix(NA,ncol = 5)

# Tx pool format:
# Col 1: block entered
# Col 2: fee
# Col 3: hash
# Col 4: tx_size, currently fixed, but will at some point be variable
# Col 5: fee multiplier

# Create a blockchain, and boostrap it with a median as pulled from the existing blockchain
blockchain <- matrix(NA,nrow=200, ncol = 10)
blockchain[,1] <- (rnorm(200, mean=51983.5, sd=20000))
hist(blockchain[,1])

# Block header format
# Col 1: block size
# Col 2: Number of transactions
# Col 3: unmodified base reward
# Col 4: Penalty
# Col 5: Total fees
# Col 6: Final block reward
# Col 7: Remaining transaction in txpool
# Col 8: Number transaction entered into pool in this block
# Col 9: Max age of transaction that was added to this block
# Col 10: Median past 100

bcnames <- c("Block Size","Transactions in block","Base Reward","Block Penalty","Fees in Block","Total Block Reward with Fees","Remaining of txs in txpool","Num transactions entered into txpool","Oldest.tx","Median100")

tmpltnames <- c("Blocktime added to txpool","Fee","Hash","Size","Multiplier")


# This is the beginning of the simulated blockchain. It will run num_blocks many times.
for (i in 1:num_blocks) {
  print("Beginning of block loop")
  print(i)
  #Take care of base reward!
  base_reward <-( tot_coins - gen_coins ) * 2^-19 
  if (base_reward < 600000000000) { base_reward <- 600000000000 }
  
  # Lets generate some fake numbers!
  
  # Number of transactions being created by users and added to the txpool in this block
  num_tx <- rnorm(1, mean=tx_mean, sd=tx_sd) # Genernate the number of transactions that are getting into the mempool during this block
  if (num_tx < 0 ) {num_tx <- 0} # If the numberis below 0, then force to 0. Skews the distribution to 0,
  if (i ==1 ) {num_tx <- 10} # Prevents dimension problem if first transaction pool is 0
  
  # A random function to create large influxes of transactions that defy the normal distribution, spike
  
  spike <- runif(1) # Probability of pool dump randomizer, or just large transaction entrance
  if (spike > 0.95 ) {num_tx <- num_tx*spike_factor}
  
  num_tx=floor(num_tx*(1+sin(i/180)))
  
  #pprint("Number of transactions")
  #print(num_tx)
  
  # What is the current size of the transaction pool
  tx_pool_num <- nrow(tx_pool)

  # What is the current median of the last 100 blocks
  size_of_blockchain <- nrow(blockchain)
  last_100 <- c(blockchain[(size_of_blockchain-99):size_of_blockchain],1)
  med_100 <- as.numeric(median(last_100))
  cat(c("Original med_100 calc:",med_100) , file = "live_blockchain.txt", sep = "\n", fill = TRUE, labels = NULL, append = TRUE)
  if (med_100 < CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2) {med_100 <- CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2}

  #Lets try the per kb fee
  #The one below, as documented on the SE and on bitcointalk, doesn't seem to actually be implemented in the code
  #fee <- (base_reward/ref_base)*(min_block/block_size_limit)*fee_factor  
  #This one seems to be implemented
  # unscaled_fee_per_kb = (DYNAMIC_FEE_PER_KB_BASE_FEE * CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2 / median_block_size);
 
  # From jollymorts work: https://github.com/JollyMort/monero-research/blob/master/Monero%20Dynamic%20Block%20Size%20and%20Dynamic%20Minimum%20Fee/Monero%20Dynamic%20Block%20Size%20and%20Dynamic%20Minimum%20Fee%20-%20DRAFT.md
  
  # F_mc = (R / R_0) * (M_0 / M) * F_0
  #where R_0 = 10 monero is the reference base reward, and F_0 = 0.002 monero / kB.
  # where R is the base block reward, B the block size and M the median block size of the last 100 blocks. The penalty doesn't come in effect unless B > M_0, where M_0 = 60000 bytes is the minimum penalty-free block size. Maximum allowed block size is 2 * M, at which the full base block reward is witheld.
  R <- base_reward
  R_0 <- ref_base
  M <- med_100
  M_0 <- CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2
  F_0 <- fee_factor
  
  BASE_FEE_PER_KB <- fee_factor
  MIN_BLOCK_SIZE_MEDIAN <- CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2
  median_size <- med_100
  subsidy <- base_reward
  REFERENCE_SUBSIDY <- ref_base
  
  #Moneromoooos
  #perkb_fee <- BASE_FEE_PER_KB * MIN_BLOCK_SIZE_MEDIAN / median_size * subsidy / REFERENCE_SUBSIDY
  
  #Jollymorts
  perkb_fee = (R / R_0) * (M_0 / M) * F_0
  
  # perkb_fee <- fee_factor*CRYPTONOTE_BLOCK_GRANTED_FULL_REWARD_ZONE_V2/med_100
  mult <- default_mult
  # Lets try simulating transactions being created during this block    

    for (p in 1:num_tx) {  
    
    # These are some really quanticed transactions!
    tx_size_random <- runif(1) 
    if(tx_size_random <= 0.5) {tx_base <- 12.65
    } else if (tx_size_random > 0.5 && tx_size_random <= 0.75) {tx_base <- 49.89
    } else if (tx_size_random > 0.75 && tx_size_random <= 0.85) {tx_base <- 23.57
    } else if (tx_size_random > 0.85 && tx_size_random <= 0.90) {tx_base <- 17.06
    } else if (tx_size_random > 0.90 && tx_size_random <= 0.98) {tx_base <- 37.58
    } else if (tx_size_random > 0.98 && tx_size_random <= 1.00) {tx_base <- 62.58
    } else {tx_base <- 12.66}
    
    tx_size <- rnorm(1, mean=(tx_base*1024), sd=1024) 
    #hist(rnorm(500, (tx_base*1024), sd=256))
    
    
    if (wallet_auto_fee == TRUE) {
      # skip for now. Need to add back in if tx_pool_size > med_100
    } else {
      transaction <- c(i,mult*perkb_fee*(tx_size/1024),digest(runif(1),algo="md5"),tx_size,mult) # Add the transaction to the mempool, 
      # Where the transaction is identified by the block # its added in, the fee multiplier, a random hash, and the transaction size
      tx_pool <- rbind(tx_pool,transaction) 
    }
  }
  
  # OK, lets try to make a block and add it to the chain and delete the transactions from the transaction pool
  # This is gonna be a bitch
  
  tx_pool_copy <- tx_pool # Lets not mess with the main transaction pool. This is useless. Dunno why I copied.
  block_template <- matrix(NA, ncol = 5)
  # initiate the block template, creating a random hash, and the size comes from 0.0928*1024
  block_template[1,] <- c(0,0,digest(runif(1),algo="md5"),95.0272,0) 
  #print("Before blockfill loop")
  #print(block_template)
  
  countloop <- 0 # Was for debugging how many times the blockfill loop ran
  
  if (nrow(tx_pool_copy) == 0) {print("Tx pool empty") # Empty transactions pools cause the sim to break
    } else {
  
  # blockfill can be found here
  #  https://github.com/monero-project/monero/blob/c642d3224c65c993ded9423358c818b83b7d74b9/src/cryptonote_core/tx_pool.cpp#L609  
  # On review, that algo seems to prioritize by fee and then by age of transaction. 
    
  repeat { # This is the beginning of the block template loop
    countloop <- countloop + 1 # This was for debugging. Shows how many times it takes to fill a block
    size_block_template <- sum(as.numeric(block_template[,4]))
    cat(c("Countloop: ",countloop," Original size_block_template:",size_block_template) , file = "live_blockchain.txt", sep = "\n", fill = TRUE, labels = NULL, append = TRUE)
  # First, we'll see if there are any high fee transactions in the mempool
    
    if (nrow(tx_pool_copy) == 0) { # Introduced because buggy if txpool gets emptied by blockfilling process 
      print("Tx pool empty and detected in blockfill loop")
      break 
      } else {


    # P_current = R * ((B / M) - 1) ^ 2
    
    # Column 6: shows the difference between the current median and the new potential blocksize with the transaction added
    tx_pool_comp <- cbind(tx_pool_copy,c(M-size_block_template))
    # Column 7: This line will calculate P_current if the transaction is added to the block
    # R * ( (B / M) - 1) ^ 2
    tx_pool_comp <- cbind(tx_pool_comp,c(R * ( ( (as.numeric(tx_pool_comp[,4]) + size_block_template) / med_100) -1 ) ^ 2 ))
    # Column 8: This line will calculate the total blockreward if the transaction is added to the block template
    tx_pool_comp <- cbind(tx_pool_comp,c(as.numeric(tx_pool_comp[,2])+base_reward-as.numeric(tx_pool_comp[,7])))
    
    tx_pool_comp <- tx_pool_comp[complete.cases(tx_pool_comp),,drop=FALSE] # Get rid of the goddamned NA
    tx_pool_copy <- tx_pool_copy[complete.cases(tx_pool_copy),,drop=FALSE] # Get rid of the goddamned NA
    
    # For reference
    # P_current = R * ((B / M) - 1) ^ 2
    # F_mc = (R / R_0) * (M_0 / M) * F_0
    # where R_0 = 10 monero is the reference base reward, and F_0 = 0.002 monero / kB.
    # where R is the base block reward, B the block size and M the median block size of the last 100 blocks. The penalty doesn't come in effect unless B > M_0, where M_0 = 60000 bytes is the minimum penalty-free block size. Maximum allowed block size is 2 * M, at which the full base block reward is witheld.
    # This is used in the final if statement. Need to modify to match the other penalize calculation
  
    # TX_POOL_COMP format
    # Col 1: block entered
    # Col 2: fee
    # Col 3: hash
    # Col 4: tx_size
    # Col 5: fee multiplier 
    # Col 6: size difference: Median - new candidate block with this transaction
    # Col 7: P_current if this transaction is added to the current block template
    # Col 8: Total block reward if this transaction is added to block template
    
    #find the highest fee multiplier and subset the pool into a high multiplier pool
    highest_mult <- max(as.numeric(tx_pool_comp[,5])) # find the highest fee MULTIPLIER
    high_mult_index <- c(which(as.numeric(tx_pool_comp[,5]) == highest_mult)) # Get row indexes of all the transactions with this multiplier
    high_mult_pool <- rbind(tx_pool_comp[high_mult_index,]) # Create sub matrix of the high transactions
    block_ordered_pool <- high_mult_pool[sort.list(high_mult_pool[,1]),,drop=FALSE] # Sort the high fee subset by when they were added to the tx pool
    
    # Could be a way to do all this without copying the data. Sort a bunch of vectors or something. 
    
    # Need to use the difference between median and current blocksize to subset pool for candidates and then sort that pool by block age
    
    #cat("TX_POOL_COMP" , file = "live_blockchain.txt", sep = "\n", fill = TRUE, labels = NULL, append = TRUE)
    #write.table(tx_pool_comp, file="live_blockchain.txt", row.names=FALSE, col.names=FALSE, append = TRUE, quote=FALSE, sep = "\t")
    
    if (size_block_template + as.numeric(block_ordered_pool[1,4]) <= med_100) {
      #So this is the easiest. If the block is lower than the median, we just add most recent and highest fee (would be the most profitable)
      #print("First if")
      #print(countloop)
      #print(block_ordered_pool[1,]) # For logging
      print("First if triggered")
      new_transaction <- block_ordered_pool[1,1:5]
      P_current <- 0
      
      } else if (size_block_template + as.numeric(block_ordered_pool[1,4]) > med_100) {
        print("Second if triggered - standard addition goes over median")
        highest_gain <- max(as.numeric(tx_pool_comp[,8]))
        stuff_index <- c(which(as.numeric(tx_pool_comp[,4]) <= as.numeric(tx_pool_comp[,6])))
        if (highest_gain >= 0) {
          print("Second sub-if. A normal ordered transaction can't fit, we can't stuff another in, and there is a high gain transaction")
          high_gain_index <- c(which(as.numeric(tx_pool_comp[,8]) == highest_gain)) # Get row indexes of all the transactions with this gain
          if (length(high_gain_index) > 1) {high_gain_index <- high_gain_index[1]} # In future can make this random but screw it for now
          new_transaction <- tx_pool_comp[high_gain_index,1:5]
          P_current <- as.numeric(tx_pool_comp[high_gain_index,7])
        
        } else if (length(stuff_index >= 1)) {
          print("First sub-if. Things can still fit in the block, just not the one that should go in based on transaction age")
          stuff_pool <- tx_pool_comp[stuff_index,,drop=FALSE] # Extract the transactions that will fit
          block_ordered_stuff <- stuff_pool[sort.list(stuff_pool[,1]),,drop=FALSE] # Sort these based on their block age
          new_transaction <- block_ordered_stuff[1,1:5]
          P_current <- 0
        
        } else {
          print("Break out of blockfill because over median and no gain possible")
          break
      }
    } else { 
      print ("No blockfill conditions met at all")
      break
      }
    
    block_template <- rbind(block_template,new_transaction) # Add a transaction to the block template
    tx_index <- which(tx_pool_copy[,3] == new_transaction[3]) # Use the hash identifier to get index of transaction in original txpool
    tx_pool_copy <- tx_pool_copy[-tx_index,,drop = FALSE] # delete this transaction from the tx_pool_copy, drop=FALSe prevents the fucker from turning into a vector
    
 } # end of second else for the tx_pool being non-empty
 } # This is the end of the Repeat Loop for creating a block template
  } # end of first else for the tx_pool being non-empty
  
    newblocksize <- sum(as.numeric(block_template[,4]))
    newblocktx <- nrow(block_template)
    new_base_reward <- base_reward - P_current
    newblockfees <- sum(as.numeric(block_template[,2]))
    newblockreward <- base_reward - P_current + newblockfees

    
  #print("We made it out of the blockfill loop")
  # Block header format
  # Col 1: block size
  # Col 2: Number of transactions
  # Col 3: unmodified base reward
  # Col 4: Penalty
  # Col 5: Total fees
  # Col 6: Final block reward
  # Col 7: Remaining transaction in txpool
  # Col 8: Number transaction entered into pool in this block
  # Col 9: Max age of transaction that was added to this block
  # Col 10: Median 100
  
  # Lets add a new block to the chain !!!
  
  newblock <- c(newblocksize,newblocktx,new_base_reward,P_current,newblockfees,newblockreward,(nrow(tx_pool_copy)),num_tx,i-max(as.numeric(block_template[,1])),med_100)
  #print(newblock)
  blockchain <- rbind (blockchain,newblock)
  
  colnames(block_template)<-tmpltnames
  
  tx_pool <- tx_pool_copy
  cat("New block being added" , file = "live_blockchain.txt", sep = "\n", fill = FALSE, labels = NULL, append = TRUE)
  cat(c(bcnames,"\n"), file = "live_blockchain.txt", sep = "\t", fill = FALSE, labels = NULL, append = TRUE)
  cat(c(newblock,"\n"), file = "live_blockchain.txt", sep = "\t", fill = FALSE, labels = NULL, append = TRUE)
  cat("Block Template" , file = "live_blockchain.txt", sep = "\n", fill = TRUE, labels = NULL, append = TRUE)
  cat(c(tmpltnames,"\n"), file = "live_blockchain.txt", sep = "\t", fill = TRUE, labels = NULL, append = TRUE)
  write.table(block_template, file="live_blockchain.txt", row.names=FALSE, col.names=FALSE, append = TRUE, quote=FALSE, sep = "\t")
  
  #print("Iteration")
  #print(i)
  #print("New Block Info")
  #print(newblock)
  
  gen_coins <- gen_coins + new_base_reward
  
  # Rest all these variables
  #Sys.sleep(1)
  last <- 200+i
  if (i > 2 && as.numeric(blockchain[last,7]) > 500*(as.numeric(blockchain[last,2]))) 
  {
    print("Tx pool grew to 500 X the most recent number of transactions added to block. Stopping simulation")
    cat(c("Tx pool grew to 500 X the most recent number of transactions added to block. Stopping simulation. Size of tx_pool",nrow(tx_pool_copy)) , file = "live_blockchain.txt", sep = "\n", fill = TRUE, labels = NULL, append = TRUE)
    break
  }
} # End of primary for loop

synth_blockchain <- blockchain[complete.cases(blockchain),]


#matplot(synth_blockchain,type="l",)
#legend("bottom", inset=.05, legend=bcnames, pch=1, col=c(2,4), horiz=FALSE)

nums <- seq(1,nrow(synth_blockchain),1)
#synth_blockchain <- cbind(synth_blockchain,nums)
colnames(synth_blockchain)<-bcnames
bcdf <- data.frame(synth_blockchain,row.names=nums)

#sink()

write.table(bcdf, file = "blockchain.txt", append = FALSE, quote = TRUE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

write.table(tx_pool, file = "finaltxpool.txt", append = FALSE, quote = TRUE, sep = "\t",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

#plot(synth_blockchain[,1], type="o", col="blue")



