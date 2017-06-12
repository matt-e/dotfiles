if [[ $HOSTID == "mac" ]]
then
    READLINK=greadlink
else
    READLINK=readlink
fi

AWS_ROOT=$HOME/aws
CREDS_ROOT=$AWS_ROOT/creds

function use_aws_creds {
    local creds;
    creds=$CREDS_ROOT/$1    
    echo "Using AWS credentials for '$1'"
    EC2_CERT=$creds/cert.pem
    EC2_PRIVATE_KEY=$creds/pk.pem
    AWS_ACCESS_KEY=`cat $creds/aws_access_key`
    AWS_SECRET_KEY=`cat $creds/aws_secret_key`

    AWS_CREDENTIAL_FILE=$CREDS_ROOT/.tmp/$$-aws_credential_file
    echo "AWSAccessKeyId=`cat $creds/aws_access_key`" > $AWS_CREDENTIAL_FILE
    echo "AWSSecretKey=`cat $creds/aws_secret_key`" >> $AWS_CREDENTIAL_FILE

    find $CREDS_ROOT/.tmp -atime +7 -delete

    export EC2_CERT
    export EC2_PRIVATE_KEY
    export AWS_ACCESS_KEY
    export AWS_SECRET_KEY
    export AWS_CREDENTIAL_FILE
}

function add_aws_creds {
    local creds;
    creds=$CREDS_ROOT/$1
    aws_access_key=$2
    aws_secret_key=$3

    mkdir -p $creds
    echo ${aws_secret_key} > $creds/aws_secret_key
    echo "$aws_access_key" > $creds/aws_access_key
    echo "Added AWS credentials for user '$1' at $creds"
}

function use_default_creds {
    local default_creds=$(basename $($READLINK $CREDS_ROOT/default))
    [ -d $default_creds ] && use_aws_creds $default_creds
}

[ -d $CREDS_ROOT ] && use_default_creds

# For unified CLI
export AWS_DEFAULT_REGION=us-west-2
