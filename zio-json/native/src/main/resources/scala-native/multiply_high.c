long zio_json_multiply_high(long x, long y) {
  return x * (unsigned __int128) y >> 64;
}

unsigned long zio_json_unsigned_multiply_high(unsigned long x, unsigned long y) {
  return x * (unsigned __int128) y >> 64;
}
