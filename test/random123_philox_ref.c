#include <stdint.h>
#include "Random123/philox.h"

//* -------------------------------------------------------------------------- */
/* -------------------------------------------------------------------------- */
void philox4x32_10_ref(const int32_t ctr[4], const int32_t key[2], int32_t out[4])
{
    philox4x32_ctr_t c = {{
        (uint32_t)ctr[0], (uint32_t)ctr[1],
        (uint32_t)ctr[2], (uint32_t)ctr[3]
    }};
    philox4x32_key_t k = {{ (uint32_t)key[0], (uint32_t)key[1] }};

    philox4x32_ctr_t r = philox4x32(c, k);

    out[0] = (int32_t)r.v[0];
    out[1] = (int32_t)r.v[1];
    out[2] = (int32_t)r.v[2];
    out[3] = (int32_t)r.v[3];
}

/* -------------------------------------------------------------------------- */
/*  2x64, 10 rounds                                                           */
/* -------------------------------------------------------------------------- */
void philox2x64_10_ref(const int64_t ctr[2], const int64_t key[1], int64_t out[2])
{
    philox2x64_ctr_t c = {{ (uint64_t)ctr[0], (uint64_t)ctr[1] }};
    philox2x64_key_t k = {{ (uint64_t)key[0] }};

    philox2x64_ctr_t r = philox2x64(c, k);

    out[0] = (int64_t)r.v[0];
    out[1] = (int64_t)r.v[1];
}

/* -------------------------------------------------------------------------- */
/*  4x64, 10 rounds                                                           */
/* -------------------------------------------------------------------------- */
void philox4x64_10_ref(const int64_t ctr[4], const int64_t key[2], int64_t out[4])
{
    philox4x64_ctr_t c = {{
        (uint64_t)ctr[0], (uint64_t)ctr[1],
        (uint64_t)ctr[2], (uint64_t)ctr[3]
    }};
    philox4x64_key_t k = {{ (uint64_t)key[0], (uint64_t)key[1] }};

    philox4x64_ctr_t r = philox4x64(c, k);

    out[0] = (int64_t)r.v[0];
    out[1] = (int64_t)r.v[1];
    out[2] = (int64_t)r.v[2];
    out[3] = (int64_t)r.v[3];
}
/*  2x32, 10 rounds                                                           */
/* -------------------------------------------------------------------------- */
void philox2x32_10_ref(const int32_t ctr[2], const int32_t key[1], int32_t out[2])
{
    philox2x32_ctr_t c = {{ (uint32_t)ctr[0], (uint32_t)ctr[1] }};
    philox2x32_key_t k = {{ (uint32_t)key[0] }};

    philox2x32_ctr_t r = philox2x32(c, k);

    out[0] = (int32_t)r.v[0];
    out[1] = (int32_t)r.v[1];
}

/* -------------------------------------------------------------------------- */
