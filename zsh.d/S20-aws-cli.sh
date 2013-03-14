if [[ $HOSTID -eq "mac" ]]
then
    READLINK=readlink
else
    READLINK=readlink
fi

export AWS_ROOT=$HOME/aws
export CREDS_ROOT=$AWS_ROOT/creds
export CLI_ROOT=$AWS_ROOT/cli
export EC2_CLI_ROOT=$AWS_ROOT/cli/ec2
export RDS_CLI_ROOT=$AWS_ROOT/cli/rds
export EMR_CLI_ROOT=$AWS_ROOT/cli/emr
export IAM_CLI_ROOT=$AWS_ROOT/cli/iam
export CW_CLI_ROOT=$AWS_ROOT/cli/cloudwatch

export AMI_ALINUX_X86_64=ami-e565ba8c
export AMI_ALINUX_I386=ami-ed65ba84

function use_aws_creds {
    local creds;
    creds=$CREDS_ROOT/$1
    echo "Using AWS credentials for user '$1' from $creds"
    EC2_CERT=$creds/cert.pem
    EC2_PRIVATE_KEY=$creds/pk.pem
    AWS_ACCESS_KEY=`cat $creds/aws_access_key`
    AWS_SECRET_KEY=`cat $creds/aws_secret_key`
    ELASTIC_MAPREDUCE_ACCESS_ID=`cat $creds/aws_access_key`
    ELASTIC_MAPREDUCE_PRIVATE_KEY=`cat $creds/aws_secret_key`

    AWS_CREDENTIAL_FILE=$CREDS_ROOT/.tmp/$$-aws_credential_file
    echo "AWSAccessKeyId=`cat $creds/aws_access_key`" > $AWS_CREDENTIAL_FILE
    echo "AWSSecretKey=`cat $creds/aws_secret_key`" >> $AWS_CREDENTIAL_FILE

    find $CREDS_ROOT/.tmp -atime +7 -delete

    export EC2_CERT
    export EC2_PRIVATE_KEY
    export AWS_ACCESS_KEY
    export AWS_SECRET_KEY
    export ELASTIC_MAPREDUCE_ACCESS_ID
    export ELASTIC_MAPREDUCE_PRIVATE_KEY

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


function use_ec2_api_tools {
   local newtools;
   newtools=`$READLINK -f $EC2_CLI_ROOT/$1`
   echo "Using EC2 tools in '$newtools'"
   export EC2_HOME=$newtools
   path_prepend $EC2_HOME/bin
}

function unuse_ec2_api_tools {
   local oldtools;
   oldtools=$EC2_HOME
   path_remove $oldtools/bin
   unset $EC2_HOME
}

function switch_ec2_api_tools {
   unuse_ec2_api_tools
   use_ec2_api_tools $1
}

function use_rds_api_tools {
   local newtools;
   newtools=`$READLINK -f $RDS_CLI_ROOT/$1`
   echo "Using RDS tools in '$newtools'"
   export AWS_RDS_HOME=$newtools
   path_prepend $AWS_RDS_HOME/bin
}

function unuse_rds_api_tools {
   local oldtools;
   oldtools=$AWS_RDS_HOME
   path_remove $oldtools/bin
}

function switch_rds_api_tools {
   unuse_rds_api_tools
   use_rds_api_tools $1
}

function use_emr_api_tools {
   local newtools;
   newtools=`$READLINK -f $EMR_CLI_ROOT/$1`
   echo "Using EMR tools in '$newtools'"
   export AWS_EMR_HOME=$newtools
   path_prepend $AWS_EMR_HOME
}

function unuse_emr_api_tools {
   local oldtools;
   oldtools=$AWS_EMR_HOME
   path_remove $oldtools
}

function switch_emr_api_tools {
   unuse_emr_api_tools
   use_emr_api_tools $1
}

function use_iam_api_tools {
   local newtools;
   newtools=`$READLINK -f $IAM_CLI_ROOT/$1`
   echo "Using IAM tools in '$newtools'"
   export AWS_IAM_HOME=$newtools
   path_prepend $AWS_IAM_HOME/bin
}

function unuse_iam_api_tools {
   local oldtools;
   oldtools=$AWS_IAM_HOME
   path_remove $oldtools
}

function switch_iam_api_tools {
   unuse_iam_api_tools
   use_iam_api_tools $1
}

function use_cw_api_tools {
   local newtools;
   newtools=`$READLINK -f $CW_CLI_ROOT/$1`
   echo "Using Cloudwatch tools in '$newtools'"
   export AWS_CLOUDWATCH_HOME=$newtools
   path_prepend $AWS_CLOUDWATCH_HOME/bin
}

function unuse_cw_api_tools {
   local oldtools;
   oldtools=$AWS_CLOUDWATCH_HOME
   path_remove $oldtools/bin
}

function switch_cw_api_tools {
   unuse_cw_api_tools
   use_cw_api_tools $1
}


use_aws_creds $USER
use_ec2_api_tools "latest"
use_rds_api_tools "latest"
use_emr_api_tools "latest"
use_iam_api_tools "latest"
use_cw_api_tools "latest"
