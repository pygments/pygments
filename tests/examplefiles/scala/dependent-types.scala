trait Entry { type Key; val key: Key }
def extractKey(e: Entry): e.Key = e.key
val extractor: (e: Entry) => e.Key = extractKey
type Extractor = Function1[Entry, Entry#Key] {
  def apply(e: Entry): e.Key
}