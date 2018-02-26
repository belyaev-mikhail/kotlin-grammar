val x = this.format(*(params.map {
    when(it) {
        is Enum<*> -> it.toString()
    }
}))