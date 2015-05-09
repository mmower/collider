(ns collider.buffer
  (:require [overtone.sc.buffer :refer [ensure-buffer-active! buffer-data write-wav buffer-alloc-read]]
            [overtone.helpers.file :refer [mk-path mk-tmp-dir! rm-rf!]]))

(defn buffer-mix-to-mono
  "Synchronously create a new buffer with only one channel by mixing
   buffer b down. Mixing is implemented simply by summing successive
   samples from each channel and dividing by the number of
   channels. Therefore, for a stereo buffer, the first sample for the
   left channel is added to the first sample for the right channel and
   the result is divided by two - and so on for each sample.

   Useful for creating buffers to use with the t-grains ugen.

   Original buffer is left unaffected. Requires internal server."
  [b]
  (ensure-buffer-active! b)
  (let [n-chans (:n-channels b)
        rate    (:rate b)]
    (cond
      (= 1 n-chans) b
      :else
      (let [data          (buffer-data b)
            partitioned   (partition n-chans (seq data))
            mixed         (mapv (fn [samps] (/ (apply + samps) n-chans)) partitioned)
            tmp-file-path (mk-path (mk-tmp-dir!) "mono-file.wav")]

        (write-wav mixed tmp-file-path rate 1)
        (let [new-b (buffer-alloc-read tmp-file-path)]
          (future (rm-rf! tmp-file-path))
          new-b)))))
