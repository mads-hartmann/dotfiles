function ll
        /usr/local/opt/coreutils/bin/gls -gGh \
           --color \
           --group-directories-first \
        | awk 'NR > 1 {print $7 }'
end
