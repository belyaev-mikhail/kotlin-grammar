fun x() {
    x + if(x > 0) 3 else 4
}

/*
* this is currently parsed as (while(x > 0) x) = (--x), which is hilarious
* */
