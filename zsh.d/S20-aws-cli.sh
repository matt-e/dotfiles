export AWS_ROOT=$HOME/aws
export CREDS_ROOT=$AWS_ROOT/creds
export CLI_ROOT=$AWS_ROOT/cli
export EC2_CLI_ROOT=$AWS_ROOT/cli/ec2
export RDS_CLI_ROOT=$AWS_ROOT/cli/rds
export EMR_CLI_ROOT=$AWS_ROOT/cli/emr

export AMI_ALINUX_X86_64=ami-e565ba8c
export AMI_ALINUX_I386=ami-ed65ba84

function use_aws_creds {
    local creds;
    creds=$CREDS_ROOT/$1
    echo "Using AWS credentials for user '$1'"
    export EC2_CERT=$creds/cert.pem
    export EC2_PRIVATE_KEY=$creds/pk.pem
    export AWS_ACCESS_KEY=`cat $creds/aws_access_key`
    export AWS_SECRET_KEY=`cat $creds/aws_secret_key`
    export ELASTIC_MAPREDUCE_ACCESS_ID=`cat $creds/aws_access_key`
    export ELASTIC_MAPREDUCE_PRIVATE_KEY=`cat $creds/aws_secret_key`
}

function use_ec2_api_tools {
   local newtools;
   newtools=`readlink -f $EC2_CLI_ROOT/$1`
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
   newtools=`readlink -f $RDS_CLI_ROOT/$1`
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
   newtools=`readlink -f $EMR_CLI_ROOT/$1`
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

use_aws_creds $USER
use_ec2_api_tools "latest"
use_rds_api_tools "latest"
use_emr_api_tools "latest"
