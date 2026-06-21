package infra

import "fmt"

// The cloud emitters lower the agnostic resource graph to OpenTofu/Terraform HCL.
// The emitted HCL is consumed by EITHER `tofu` or `terraform` (the language is
// shared; only the CLI binary and provider registry differ), so one emitter per
// provider serves both IaC tools. Each emitter realizes EXACTLY the agnostic
// LogicalResource set — the cloud-specific scaffolding (resource groups, Service Bus
// namespaces, storage accounts, Pub/Sub subscriptions) is provider plumbing, not
// part of the logical set the equivalence gate compares.

// unsupported reports a resource kind a target does not lower. A clear error, never
// a panic.
func unsupported(target string, r Resource) error {
	return fmt.Errorf("infra: target %q does not yet support the %q resource %q",
		target, r.Kind(), r.LogicalName())
}

// hasKind reports whether the graph contains a resource of the given agnostic kind
// (drives which shared provider scaffolding to emit).
func hasKind(rs []Resource, kind string) bool {
	for _, r := range rs {
		if r.Kind() == kind {
			return true
		}
	}
	return false
}

// needsKeyVault reports whether the Azure graph needs the shared Key Vault (both
// Secret and KMS hang off it).
func needsKeyVault(rs []Resource) bool {
	return hasKind(rs, "secret") || hasKind(rs, "kms")
}

// needsStorageAccount reports whether the Azure graph needs the shared storage
// account (both Bucket/object and File hang off it).
func needsStorageAccount(rs []Resource) bool {
	return hasKind(rs, "object") || hasKind(rs, "file")
}

// tfFile wraps an HCL string as the standard single-file artifact.
func tfFile(h *hcl, rs []Resource) Artifact {
	return Artifact{Files: map[string]string{"main.tf": h.String()}, Logical: logicalSet(rs)}
}

// AWS lowers to OpenTofu/Terraform HCL for Amazon Web Services.
type AWS struct{}

func (AWS) Target() string { return "aws" }
func (AWS) Cloud() bool    { return true }

func (AWS) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("aws =")
	h.attr("source", str("hashicorp/aws"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"aws_region\"")
	h.attr("type", "string")
	h.attr("default", str("us-east-1"))
	h.close()
	if hasKind(rs, "compute") {
		h.blank()
		h.open("variable \"aws_ami\"")
		h.attr("type", "string")
		h.attr("default", str("ami-0c101f26f147fa7fd"))
		h.close()
	}
	if hasKind(rs, "database") {
		h.blank()
		h.open("variable \"db_password\"")
		h.attr("type", "string")
		h.attr("sensitive", "true")
		h.close()
	}
	if hasKind(rs, "secret") {
		h.blank()
		h.open("variable \"secret_value\"")
		h.attr("type", "string")
		h.attr("sensitive", "true")
		h.close()
	}
	h.blank()
	h.open("provider \"aws\"")
	h.attr("region", "var.aws_region")
	h.close()
	for _, r := range rs {
		h.blank()
		switch v := r.(type) {
		case Queue:
			h.open("resource \"aws_sqs_queue\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			if v.FIFO {
				h.attr("fifo_queue", "true")
			}
			h.close()
		case Compute:
			h.open("resource \"aws_instance\" %s", str(v.Name))
			h.attr("count", fmt.Sprintf("%d", v.replicaCount()))
			h.attr("ami", "var.aws_ami")
			h.attr("instance_type", str("t3.micro"))
			h.attr("user_data", str("#!/bin/sh\npodman run -d "+v.imageRef()+"\n"))
			h.close()
		case KV:
			h.open("resource \"aws_elasticache_cluster\" %s", str(v.Name))
			h.attr("cluster_id", str(v.Name))
			h.attr("engine", str("redis"))
			h.attr("node_type", str("cache.t3.micro"))
			h.attr("num_cache_nodes", "1")
			h.close()
		case Bucket:
			h.open("resource \"aws_s3_bucket\" %s", str(v.Name))
			h.attr("bucket", str(v.Name))
			h.close()
		case Database:
			h.open("resource \"aws_db_instance\" %s", str(v.Name))
			h.attr("identifier", str(v.Name))
			h.attr("engine", str("postgres"))
			h.attr("instance_class", str("db.t3.micro"))
			h.attr("allocated_storage", "20")
			h.attr("username", str("wavelet"))
			h.attr("password", "var.db_password")
			h.attr("skip_final_snapshot", "true")
			h.close()
		case Secret:
			h.open("resource \"aws_secretsmanager_secret\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.close()
			h.blank()
			h.open("resource \"aws_secretsmanager_secret_version\" %s", str(v.Name))
			h.attr("secret_id", "aws_secretsmanager_secret."+v.Name+".id")
			h.attr("secret_string", "var.secret_value")
			h.close()
		case NoSQL:
			h.open("resource \"aws_dynamodb_table\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("billing_mode", str("PAY_PER_REQUEST"))
			h.attr("hash_key", str("id"))
			h.open("attribute")
			h.attr("name", str("id"))
			h.attr("type", str("S"))
			h.close()
			h.close()
		case DNS:
			h.open("resource \"aws_route53_zone\" %s", str(v.Name))
			h.attr("name", str(v.domain()))
			h.close()
		case Disk:
			h.open("resource \"aws_ebs_volume\" %s", str(v.Name))
			h.attr("availability_zone", "\"${var.aws_region}a\"")
			h.attr("size", fmt.Sprintf("%d", v.sizeGB()))
			h.close()
		case KMS:
			h.open("resource \"aws_kms_key\" %s", str(v.Name))
			h.attr("description", str("wavelet "+v.Name))
			h.close()
			h.blank()
			h.open("resource \"aws_kms_alias\" %s", str(v.Name))
			h.attr("name", str("alias/"+v.Name))
			h.attr("target_key_id", "aws_kms_key."+v.Name+".key_id")
			h.close()
		case File:
			h.open("resource \"aws_efs_file_system\" %s", str(v.Name))
			h.attr("creation_token", str(v.Name))
			h.close()
		case Stream:
			h.open("resource \"aws_kinesis_stream\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("shard_count", "1")
			h.close()
		default:
			return Artifact{}, unsupported("aws", r)
		}
	}
	return tfFile(h, rs), nil
}

// Azure lowers to OpenTofu/Terraform HCL for Microsoft Azure. Azure resources hang
// off a resource group; Service Bus needs a namespace, Blob needs a storage account
// (all shared plumbing, emitted once when a resource needs it).
type Azure struct{}

func (Azure) Target() string { return "azure" }
func (Azure) Cloud() bool    { return true }

func (Azure) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("azurerm =")
	h.attr("source", str("hashicorp/azurerm"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"azure_location\"")
	h.attr("type", "string")
	h.attr("default", str("eastus"))
	h.close()
	if hasKind(rs, "compute") {
		h.blank()
		h.open("variable \"azure_ssh_public_key\"")
		h.attr("type", "string")
		h.close()
	}
	if hasKind(rs, "database") {
		h.blank()
		h.open("variable \"db_password\"")
		h.attr("type", "string")
		h.attr("sensitive", "true")
		h.close()
	}
	if needsKeyVault(rs) {
		h.blank()
		h.open("variable \"azure_tenant_id\"")
		h.attr("type", "string")
		h.close()
	}
	if hasKind(rs, "secret") {
		h.blank()
		h.open("variable \"secret_value\"")
		h.attr("type", "string")
		h.attr("sensitive", "true")
		h.close()
	}
	h.blank()
	h.open("provider \"azurerm\"")
	h.emptyBlock("features")
	h.close()
	h.blank()
	h.open("resource \"azurerm_resource_group\" \"wavelet\"")
	h.attr("name", str("wavelet-rg"))
	h.attr("location", "var.azure_location")
	h.close()
	if hasKind(rs, "compute") {
		h.blank()
		h.open("resource \"azurerm_virtual_network\" \"wavelet\"")
		h.attr("name", str("wavelet-vnet"))
		h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
		h.attr("location", "azurerm_resource_group.wavelet.location")
		h.attr("address_space", "[\"10.0.0.0/16\"]")
		h.close()
		h.blank()
		h.open("resource \"azurerm_subnet\" \"wavelet\"")
		h.attr("name", str("wavelet-subnet"))
		h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
		h.attr("virtual_network_name", "azurerm_virtual_network.wavelet.name")
		h.attr("address_prefixes", "[\"10.0.1.0/24\"]")
		h.close()
	}
	if hasKind(rs, "queue") {
		h.blank()
		h.open("resource \"azurerm_servicebus_namespace\" \"wavelet\"")
		h.attr("name", str("wavelet-ns"))
		h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
		h.attr("location", "azurerm_resource_group.wavelet.location")
		h.attr("sku", str("Standard"))
		h.close()
	}
	if hasKind(rs, "stream") {
		h.blank()
		h.open("resource \"azurerm_eventhub_namespace\" \"wavelet\"")
		h.attr("name", str("wavelet-ehns"))
		h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
		h.attr("location", "azurerm_resource_group.wavelet.location")
		h.attr("sku", str("Standard"))
		h.close()
	}
	if needsStorageAccount(rs) {
		h.blank()
		h.open("resource \"azurerm_storage_account\" \"wavelet\"")
		h.attr("name", str("waveletstorage"))
		h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
		h.attr("location", "azurerm_resource_group.wavelet.location")
		h.attr("account_tier", str("Standard"))
		h.attr("account_replication_type", str("LRS"))
		h.close()
	}
	if needsKeyVault(rs) {
		h.blank()
		h.open("resource \"azurerm_key_vault\" \"wavelet\"")
		h.attr("name", str("wavelet-kv"))
		h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
		h.attr("location", "azurerm_resource_group.wavelet.location")
		h.attr("tenant_id", "var.azure_tenant_id")
		h.attr("sku_name", str("standard"))
		h.close()
	}
	for _, r := range rs {
		h.blank()
		switch v := r.(type) {
		case Queue:
			h.open("resource \"azurerm_servicebus_queue\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("namespace_id", "azurerm_servicebus_namespace.wavelet.id")
			if v.FIFO {
				h.attr("requires_session", "true")
			}
			h.close()
		case Compute:
			cnt := fmt.Sprintf("%d", v.replicaCount())
			h.open("resource \"azurerm_network_interface\" %s", str(v.Name))
			h.attr("count", cnt)
			h.attr("name", fmt.Sprintf("%q", v.Name+"-nic-${count.index}"))
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.attr("location", "azurerm_resource_group.wavelet.location")
			h.open("ip_configuration")
			h.attr("name", str("internal"))
			h.attr("subnet_id", "azurerm_subnet.wavelet.id")
			h.attr("private_ip_address_allocation", str("Dynamic"))
			h.close()
			h.close()
			h.blank()
			h.open("resource \"azurerm_linux_virtual_machine\" %s", str(v.Name))
			h.attr("count", cnt)
			h.attr("name", fmt.Sprintf("%q", v.Name+"-${count.index}"))
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.attr("location", "azurerm_resource_group.wavelet.location")
			h.attr("size", str("Standard_B1s"))
			h.attr("admin_username", str("wavelet"))
			h.attr("network_interface_ids", "[azurerm_network_interface."+v.Name+"[count.index].id]")
			h.open("admin_ssh_key")
			h.attr("username", str("wavelet"))
			h.attr("public_key", "var.azure_ssh_public_key")
			h.close()
			h.open("os_disk")
			h.attr("caching", str("ReadWrite"))
			h.attr("storage_account_type", str("Standard_LRS"))
			h.close()
			h.open("source_image_reference")
			h.attr("publisher", str("Canonical"))
			h.attr("offer", str("0001-com-ubuntu-server-jammy"))
			h.attr("sku", str("22_04-lts"))
			h.attr("version", str("latest"))
			h.close()
			h.close()
		case KV:
			h.open("resource \"azurerm_redis_cache\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.attr("location", "azurerm_resource_group.wavelet.location")
			h.attr("capacity", "1")
			h.attr("family", str("C"))
			h.attr("sku_name", str("Basic"))
			h.close()
		case Bucket:
			h.open("resource \"azurerm_storage_container\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("storage_account_name", "azurerm_storage_account.wavelet.name")
			h.attr("container_access_type", str("private"))
			h.close()
		case Database:
			h.open("resource \"azurerm_postgresql_flexible_server\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.attr("location", "azurerm_resource_group.wavelet.location")
			h.attr("version", str("15"))
			h.attr("administrator_login", str("wavelet"))
			h.attr("administrator_password", "var.db_password")
			h.attr("sku_name", str("B_Standard_B1ms"))
			h.attr("storage_mb", "32768")
			h.close()
		case Secret:
			h.open("resource \"azurerm_key_vault_secret\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("value", "var.secret_value")
			h.attr("key_vault_id", "azurerm_key_vault.wavelet.id")
			h.close()
		case NoSQL:
			h.open("resource \"azurerm_cosmosdb_account\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.attr("location", "azurerm_resource_group.wavelet.location")
			h.attr("offer_type", str("Standard"))
			h.attr("kind", str("GlobalDocumentDB"))
			h.open("consistency_policy")
			h.attr("consistency_level", str("Session"))
			h.close()
			h.open("geo_location")
			h.attr("location", "azurerm_resource_group.wavelet.location")
			h.attr("failover_priority", "0")
			h.close()
			h.close()
		case DNS:
			h.open("resource \"azurerm_dns_zone\" %s", str(v.Name))
			h.attr("name", str(v.domain()))
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.close()
		case Disk:
			h.open("resource \"azurerm_managed_disk\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.attr("location", "azurerm_resource_group.wavelet.location")
			h.attr("storage_account_type", str("Standard_LRS"))
			h.attr("create_option", str("Empty"))
			h.attr("disk_size_gb", fmt.Sprintf("%d", v.sizeGB()))
			h.close()
		case KMS:
			h.open("resource \"azurerm_key_vault_key\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("key_vault_id", "azurerm_key_vault.wavelet.id")
			h.attr("key_type", str("RSA"))
			h.attr("key_size", "2048")
			h.attr("key_opts", "[\"encrypt\", \"decrypt\"]")
			h.close()
		case File:
			h.open("resource \"azurerm_storage_share\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("storage_account_name", "azurerm_storage_account.wavelet.name")
			h.attr("quota", "50")
			h.close()
		case Stream:
			h.open("resource \"azurerm_eventhub\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("namespace_name", "azurerm_eventhub_namespace.wavelet.name")
			h.attr("resource_group_name", "azurerm_resource_group.wavelet.name")
			h.attr("partition_count", "2")
			h.attr("message_retention", "1")
			h.close()
		default:
			return Artifact{}, unsupported("azure", r)
		}
	}
	return tfFile(h, rs), nil
}

// GCP lowers to OpenTofu/Terraform HCL for Google Cloud.
type GCP struct{}

func (GCP) Target() string { return "gcp" }
func (GCP) Cloud() bool    { return true }

func (GCP) Emit(rs []Resource) (Artifact, error) {
	h := &hcl{}
	h.open("terraform")
	h.open("required_providers")
	h.open("google =")
	h.attr("source", str("hashicorp/google"))
	h.close()
	h.close()
	h.close()
	h.blank()
	h.open("variable \"gcp_project\"")
	h.attr("type", "string")
	h.close()
	h.blank()
	h.open("variable \"gcp_region\"")
	h.attr("type", "string")
	h.attr("default", str("us-central1"))
	h.close()
	h.blank()
	h.open("variable \"gcp_zone\"")
	h.attr("type", "string")
	h.attr("default", str("us-central1-a"))
	h.close()
	h.blank()
	h.open("provider \"google\"")
	h.attr("project", "var.gcp_project")
	h.attr("region", "var.gcp_region")
	h.close()
	for _, r := range rs {
		h.blank()
		switch v := r.(type) {
		case Queue:
			h.open("resource \"google_pubsub_topic\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.close()
			h.blank()
			h.open("resource \"google_pubsub_subscription\" %s", str(v.Name+"_sub"))
			h.attr("name", str(v.Name+"-sub"))
			h.attr("topic", "google_pubsub_topic."+v.Name+".name")
			h.close()
		case Compute:
			h.open("resource \"google_compute_instance\" %s", str(v.Name))
			h.attr("count", fmt.Sprintf("%d", v.replicaCount()))
			h.attr("name", fmt.Sprintf("%q", v.Name+"-${count.index}"))
			h.attr("machine_type", str("e2-micro"))
			h.attr("zone", "var.gcp_zone")
			h.open("boot_disk")
			h.open("initialize_params")
			h.attr("image", str("debian-cloud/debian-12"))
			h.close()
			h.close()
			h.open("network_interface")
			h.attr("network", str("default"))
			h.emptyBlock("access_config")
			h.close()
			h.attr("metadata_startup_script", str("podman run -d "+v.imageRef()))
			h.close()
		case KV:
			h.open("resource \"google_redis_instance\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("memory_size_gb", "1")
			h.close()
		case Bucket:
			h.open("resource \"google_storage_bucket\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("location", "var.gcp_region")
			h.close()
		case Database:
			h.open("resource \"google_sql_database_instance\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("database_version", str("POSTGRES_15"))
			h.attr("deletion_protection", "false")
			h.open("settings")
			h.attr("tier", str("db-f1-micro"))
			h.close()
			h.close()
		case Secret:
			h.open("resource \"google_secret_manager_secret\" %s", str(v.Name))
			h.attr("secret_id", str(v.Name))
			h.open("replication")
			h.emptyBlock("auto")
			h.close()
			h.close()
			h.blank()
			h.open("resource \"google_secret_manager_secret_version\" %s", str(v.Name))
			h.attr("secret", "google_secret_manager_secret."+v.Name+".id")
			h.attr("secret_data", "var.secret_value")
			h.close()
		case NoSQL:
			h.open("resource \"google_firestore_database\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("location_id", "var.gcp_region")
			h.attr("type", str("FIRESTORE_NATIVE"))
			h.close()
		case DNS:
			h.open("resource \"google_dns_managed_zone\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("dns_name", str(v.domain()+"."))
			h.close()
		case Disk:
			h.open("resource \"google_compute_disk\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("size", fmt.Sprintf("%d", v.sizeGB()))
			h.attr("zone", "var.gcp_zone")
			h.close()
		case KMS:
			h.open("resource \"google_kms_key_ring\" %s", str(v.Name))
			h.attr("name", str(v.Name+"-ring"))
			h.attr("location", "var.gcp_region")
			h.close()
			h.blank()
			h.open("resource \"google_kms_crypto_key\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("key_ring", "google_kms_key_ring."+v.Name+".id")
			h.close()
		case File:
			h.open("resource \"google_filestore_instance\" %s", str(v.Name))
			h.attr("name", str(v.Name))
			h.attr("location", "var.gcp_zone")
			h.attr("tier", str("BASIC_HDD"))
			h.open("file_shares")
			h.attr("name", str("share1"))
			h.attr("capacity_gb", "1024")
			h.close()
			h.open("networks")
			h.attr("network", str("default"))
			h.attr("modes", "[\"MODE_IPV4\"]")
			h.close()
			h.close()
		case Stream:
			h.open("resource \"google_pubsub_topic\" %s", str(v.Name+"_stream"))
			h.attr("name", str(v.Name))
			h.attr("message_retention_duration", str("86400s"))
			h.close()
		default:
			return Artifact{}, unsupported("gcp", r)
		}
	}
	return tfFile(h, rs), nil
}
