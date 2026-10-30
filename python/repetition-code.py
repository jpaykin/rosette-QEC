import stim
import numpy as np

# Create a circuit with 3 data qubits

d = 3
p = 0.1

c = stim.Circuit()

# prepare |0_L> = |000>
c.append("R", range(d))

# inject errors here
# traditionally we'd choose just X errors, but here any errors
c.append("DEPOLARIZE1", range(d), p)

# measure data
c.append("M", range(d))

# syndrome checks
for i in range(d - 1):
    c.append(
        "DETECTOR",
        [
            stim.target_rec(-(d - i)),
            stim.target_rec(-(d - i - 1)),
        ]
    )

# observable
c.append(
    "OBSERVABLE_INCLUDE",
    [stim.target_rec(-d)],
    0,
)

shots = 1_000_000

sampler = c.compile_detector_sampler()
dets, obs = sampler.sample(
    shots,
    separate_observables=True,
)

print("DETECTOR SAMPLES")
print(dets)
print("\n")

print("OBSERVABLE SAMPLES")
print(obs)
print("\n")

zero_syndrome = ~np.any(dets, axis=1)
logical_flip = obs[:, 0]

logical_error = zero_syndrome & logical_flip

print("estimated pL =", np.mean(logical_error))